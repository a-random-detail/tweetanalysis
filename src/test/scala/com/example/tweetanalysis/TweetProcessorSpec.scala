package com.example.tweetanalysis

import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import org.specs2.matcher.MatchResult
import fs2.Stream

class TweetProcessorSpec extends org.specs2.mutable.Specification {

  "TweetProcessor" >> {
    "return stream of strings as output" >> {
      returnsStdoutStream()
    }
  }

  private[this] def returnProcessingResult(input: Stream[IO, Tweet]): List[String] = TweetProcessor.impl[IO].analyze(input).compile.toVector.unsafeRunSync().toList

  private[this] def returnsStdoutStream(): MatchResult[Boolean] = {
    val input = Stream(
      new Tweet("1970-01-01", "First test tweet",  new Entities(List[Hashtag]())),
      new Tweet("1970-02-07", "First test tweet",  new Entities(List[Hashtag]())),
      new Tweet("1970-03-06", "First test tweet",  new Entities(List[Hashtag]())),
    )
    val expectedOutput = List.fill(3)("processed tweet")
    returnProcessingResult(input).forall(expectedOutput.contains(_)) must beTrue
  }
}
