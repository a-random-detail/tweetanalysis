package com.example.tweetanalysis

import fs2.Stream
import cats.effect.Sync
import java.time._

trait TweetProcessor[F[_]] {
  def analyze(s: Stream[F, Tweet]): Stream[F, AnalysisResult]
}

case class AnalysisResult(totalTweets: Int, tweetsPerSecond: Double, tweetsPerMinute: Double, tweetsPerHour: Double)
object TweetProcessor {
  def impl[F[_]: Sync]: TweetProcessor[F] = new TweetProcessor[F] {
    val startTime = LocalTime.now()
    def analyze(s: Stream[F, Tweet]): Stream[F, AnalysisResult] = {
      val count = s.scan(0)((acc,_) => acc+1).drop(1)
      val perSecondStream = count.map(_.toDouble / timeDeltaSeconds)
      count.zipWith(perSecondStream)((a,b) => AnalysisResult(a,b, b*60, b*60*60))
    }
    private def timeDeltaSeconds: Double = Duration.between(startTime, LocalTime.now()).toMillis.toDouble / 1000
  }
}
