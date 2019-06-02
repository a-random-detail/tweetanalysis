package com.example.tweetanalysis

import fs2.Stream
import cats.effect.Sync

trait TweetProcessor[F[_]] {
  def analyze(s: Stream[F, Tweet]): Stream[F, String]
}


object TweetProcessor {
  def impl[F[_]: Sync]: TweetProcessor[F] = new TweetProcessor[F] {
    def analyze(s: Stream[F, Tweet]): Stream[F, String] = s.map{x => "tweet processor analysis output"}
  }
}
