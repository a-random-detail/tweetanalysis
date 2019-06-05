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

case class ProcessedMetadata(topHashtags: Map[Hashtag, Int], hasUrls: Boolean, hasPhotoUrls: Boolean)
object TweetProcessor {
  def impl[F[_]: Sync]: TweetProcessor[F] = new TweetProcessor[F] {
    val startTime = LocalTime.now()
    def analyze(s: Stream[F, Tweet]): Stream[F, AnalysisResult] = {
      def metadata =
        s.map(TweetMetadata.get(_))
          .scan(ProcessedMetadata(Map[Hashtag, Int](), false, false))((acc, next) => {
            val nextMap = next.hashtags.groupBy(i => i).mapValues(_.size)
            ProcessedMetadata(acc.topHashtags.combine(nextMap), next.urls.length > 0, next.photoUrls.length > 0)
          })
          .drop(1)

      def counter = StreamCounter.impl[F](startTime).getTweetMetadataCounts(metadata)

      counter.zipWith(metadata)((a, b) => {
        val tweetsPerSecond = a.total.toDouble / a.timeElapsed
        val urlPercentage = roundPercentage(a.urlCount.toDouble / a.total.toDouble)
        val photoUrlPercentage = roundPercentage(a.photoUrlCount.toDouble / a.total.toDouble)
        AnalysisResult(a.total,
                       tweetsPerSecond,
                       tweetsPerSecond * 60,
                       tweetsPerSecond * 60 * 60,
                       a.timeElapsed,
                       retrieveTopHashtags(b.topHashtags),
                       urlPercentage,
                       photoUrlPercentage)
      })
    }
    private def roundPercentage(d: Double) = Math.round(d * 10000.0) / 100.0
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


  }
}
