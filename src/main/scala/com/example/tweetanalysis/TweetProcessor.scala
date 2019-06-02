package com.example.tweetanalysis

import fs2.Stream
import cats.effect.Sync

trait TweetProcessor[F[_]] {
  def analyze(s: Stream[F, Tweet]): Stream[F, AnalysisResult]
}

case class AnalysisResult(totalTweets: Int)
object TweetProcessor {
  def impl[F[_]: Sync]: TweetProcessor[F] = new TweetProcessor[F] {
    def analyze(s: Stream[F, Tweet]): Stream[F, AnalysisResult] = s.scan(0)((acc,_) => acc+1).drop(1).map(AnalysisResult(_))
  }
}
