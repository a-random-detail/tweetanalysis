package com.example.tweetanalysis

import fs2.Stream
import cats.effect.Sync
import cats.implicits._
import java.time._
import scala.collection.SortedMap

trait TweetProcessor[F[_]] {
  def analyze(s: Stream[F, Tweet]): Stream[F, AnalysisResult]
}

case class AnalysisResult(totalTweets: Int,
                          tweetsPerSecond: Double,
                          tweetsPerMinute: Double,
                          tweetsPerHour: Double,
                          timeElapsed: Double,
                          topHashtags: Map[Hashtag, Int])

object TweetProcessor {
  def impl[F[_]: Sync]: TweetProcessor[F] = new TweetProcessor[F] {
    val startTime = LocalTime.now()
    def analyze(s: Stream[F, Tweet]): Stream[F, AnalysisResult] = {
      def count =
        s.scan((0, 0.0))((acc, _) => {
            val total = acc._1 + 1
            (total, timeDeltaSeconds)
          })
          .drop(1)

      def hashtagTotals =
        s.map(TweetMetadata.get(_))
          .scan(Map[Hashtag, Int]())((acc, next) =>
            acc.combine(next.hashtags.groupBy(i => i).mapValues(_.size)))
          .drop(1)
      count.zipWith(hashtagTotals)((a, b) => {
        val (total, timeElapsed) = a
        val tweetsPerSecond = total.toDouble / timeElapsed
        AnalysisResult(total,
                       tweetsPerSecond,
                       tweetsPerSecond * 60,
                       tweetsPerSecond * 60 * 60,
                       timeElapsed,
                       retrieveTopHashtags(b))
      })
    }

    private def retrieveTopHashtags(m: Map[Hashtag, Int]): Map[Hashtag, Int] =
      m.toList
        .sortWith((a, b) => {
          if (a._2 == b._2) {
            a._1.text < b._1.text
          } else {
            a._2 > b._2
          }
        })
        .take(3)
        .toMap

    private def timeDeltaSeconds: Double =
      Duration.between(startTime, LocalTime.now()).toMillis.toDouble / 1000
  }
}
