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
                          topHashtags: Map[Hashtag, Int],
                          percentageContainingUrl: Double,
                          percentageContainingPhotoUrl: Double)

object TweetProcessor {
  def impl[F[_]: Sync]: TweetProcessor[F] = new TweetProcessor[F] {
    val startTime = LocalTime.now()
    def analyze(s: Stream[F, Tweet]): Stream[F, AnalysisResult] = {
      def metadata =
        s.map(TweetMetadata.get(_))
          .scan((Map[Hashtag, Int](), false))((acc, next) => {
            val nextMap = next.hashtags.groupBy(i => i).mapValues(_.size)
            (acc._1.combine(nextMap), next.urls.length > 0)
          })
          .drop(1)
      def count =
        metadata
          .scan((0, 0.0, 0))((acc, next) => {
            val total = acc._1 + 1
            val incrementUrls = if (next._2) acc._3 + 1 else acc._3
            (total, timeDeltaSeconds, incrementUrls)
          })
          .drop(1)
      count.zipWith(metadata)((a, b) => {
        val (total, timeElapsed, numberContainingUrls) = a
        val tweetsPerSecond = total.toDouble / timeElapsed
        val urlPercentage = Math.round(
          numberContainingUrls.toDouble / total.toDouble * 10000.0) / 100.0
        AnalysisResult(total,
                       tweetsPerSecond,
                       tweetsPerSecond * 60,
                       tweetsPerSecond * 60 * 60,
                       timeElapsed,
                       retrieveTopHashtags(b._1),
                       urlPercentage,
                       0.0)
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
