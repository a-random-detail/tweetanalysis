package com.example.tweetanalysis

import fs2.Stream
import cats.effect.Sync
import cats.implicits._
import java.time._
import scala.collection.SortedMap

trait StreamCounter[F[_]] {
  def getTweetMetadataCounts(s: Stream[F, ProcessedMetadata]): Stream[F, StreamCounterResult]
}

case class StreamCounterResult(total: Int, timeElapsed: Double, urlCount: Int, photoUrlCount: Int)

object StreamCounter {
  def impl[F[_]: Sync](startTime: LocalTime): StreamCounter[F] = new StreamCounter[F] {
    def getTweetMetadataCounts(s: Stream[F, ProcessedMetadata]): Stream[F, StreamCounterResult] =
      s.scan(StreamCounterResult(0, 0.0, 0, 0))((acc, next) =>  next match {
        case ProcessedMetadata(_, true, true) => StreamCounterResult(acc.total+1, timeDeltaSeconds, acc.urlCount + 1, acc.photoUrlCount + 1)
        case ProcessedMetadata(_, true, false) => StreamCounterResult(acc.total+1, timeDeltaSeconds, acc.urlCount + 1, acc.photoUrlCount)
        case ProcessedMetadata(_, false, true) => StreamCounterResult(acc.total+1, timeDeltaSeconds, acc.urlCount, acc.photoUrlCount + 1)
        case _ => StreamCounterResult(acc.total+1, timeDeltaSeconds, acc.urlCount, acc.photoUrlCount) 
      })
      .drop(1)
    private def timeDeltaSeconds: Double =
        Duration.between(startTime, LocalTime.now()).toMillis.toDouble / 1000
  }
}
