package com.example.tweetanalysis

import org.http4s._
import org.http4s.client.blaze._
import org.http4s.client.oauth1
import cats.effect._
import cats.implicits._
import fs2.Stream
import fs2.io.stdout
import fs2.text.{lines, utf8Encode}
import jawnfs2._
import java.util.concurrent.{Executors, ExecutorService}
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global
import io.circe.{Encoder, Decoder, Json, HCursor}
import java.time._


class TWStream[F[_]](implicit F: ConcurrentEffect[F], cs: ContextShift[F]) {
  // jawn-fs2 needs to know what JSON AST you want
  implicit val f = io.circe.jawn.CirceSupportParser.facade


  def sign(consumerKey: String, consumerSecret: String, accessToken: String, accessSecret: String)
          (req: Request[F]): F[Request[F]] = {
    val consumer = oauth1.Consumer(consumerKey, consumerSecret)
    val token    = oauth1.Token(accessToken, accessSecret)
    oauth1.signRequest(req, consumer, callback = None, verifier = None, token = Some(token))
  }


  def jsonStream(consumerKey: String, consumerSecret: String, accessToken: String, accessSecret: String)
            (req: Request[F]): Stream[F, Json] =
    for {
      client <- BlazeClientBuilder(global).stream
      sr  <- Stream.eval(sign(consumerKey, consumerSecret, accessToken, accessSecret)(req))
      res <- client.stream(sr).flatMap(_.body.chunks.parseJsonStream)
    } yield res


  def stream(blockingEC: ExecutionContext): Stream[F, Unit] = {
    val processor = TweetProcessor.impl[F]
    val req = Request[F](Method.GET, Uri.uri("https://stream.twitter.com/1.1/statuses/sample.json"))
    def s   = jsonStream("2ijtWnTf0QTWIXdAk7iO5ttF6", "z3x375TZ1WubFGHx8iJrvgOsxWBQtDDBnvvDnmc8dBJ59x7BpX", "1133841180507234304-fcQzIoHTuvxxiAlbSGjiZMjMlwDEHn", "LszzxehNhW4Mu9EOMy8cLqKVBLyxKBy7p5rhVTV7sbMhp")(req)

    def tweets = s.map(_.as[Tweet]).collect { case Right(x) => x }.take(50)
      processor.analyze(tweets)
      .map(x => {
        s"""
        ---------------
        total tweets: ${x.totalTweets}
        time elapsed: ${x.timeElapsed}
        tweets per:
          second: ${x.tweetsPerSecond}
          minute: ${x.tweetsPerMinute}
          hour:   ${x.tweetsPerHour}
        top hashtags: ${x.topHashtags}
        ---------------
        """
      })
      .through(utf8Encode)
      .through(stdout(blockingEC))
    }

  /**
   * We're going to be writing to stdout, which is a blocking API.  We don't
   * want to block our main threads, so we create a separate pool.  We'll use
   * `fs2.Stream` to manage the shutdown for us.
   */
  def blockingEcStream: Stream[F, ExecutionContext] =
    Stream.bracket(F.delay(Executors.newFixedThreadPool(4)))(pool =>
        F.delay(pool.shutdown()))
      .map(ExecutionContext.fromExecutorService)

  /** Compile our stream down to an effect to make it runnable */
  def run: F[Unit] =
    blockingEcStream.flatMap { blockingEc =>
      stream(blockingEc)
    }.compile.drain
}
