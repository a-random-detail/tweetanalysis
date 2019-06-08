package com.example.tweetanalysis

import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import org.specs2.matcher.MatchResult
import fs2.Stream
import java.time._

class StreamCounterSpec extends org.specs2.mutable.Specification {

  "StreamCounter" >> {
    "returns correct running tweet total " >> {
      returnsRunningTweetTotal()
    }
    "returns correct url count" >> {
      returnsUrlCount()
    }
    "returns correct photo url count" >> {
      returnsPhotoUrlCount()
    }
    "returns correct emoji containing tweet count" >> {
      returnsEmojiTweetCount()
    }
  }

  private[this] def returnImplementation: StreamCounter[IO] = StreamCounter.impl[IO](LocalTime.now())

  private[this] def returnsRunningTweetTotal(): MatchResult[List[Int]] = {
    val inputStream = Stream(
      new ProcessedMetadata(Map(), Map(), Map(), false, false, false),
      new ProcessedMetadata(Map(), Map(), Map(), false, false, false),
      new ProcessedMetadata(Map(), Map(), Map(), false, false, false),
      new ProcessedMetadata(Map(), Map(), Map(), false, false, false)
    )

    val actual = returnImplementation.getTweetMetadataCounts(inputStream).map(_.total).compile.toVector
      .unsafeRunSync()
      .toList

    actual must beEqualTo(List(1,2,3,4))
  }
  private[this] def returnsUrlCount(): MatchResult[List[Int]] = {
    val inputStream = Stream(
      new ProcessedMetadata(Map(), Map(), Map(), false, false, false),
      new ProcessedMetadata(Map(), Map(), Map(), true, false, false),
      new ProcessedMetadata(Map(), Map(), Map(), true, false, false),
      new ProcessedMetadata(Map(), Map(), Map(), false, false, false),
    )
    val actual = returnImplementation.getTweetMetadataCounts(inputStream).map(_.urlCount).compile.toVector
      .unsafeRunSync()
      .toList

    actual must beEqualTo(List(0,1,2,2))
  }

  private[this] def returnsPhotoUrlCount(): MatchResult[List[Int]] = {
    val inputStream = Stream(
      new ProcessedMetadata(Map(), Map(), Map(), false, false, false),
      new ProcessedMetadata(Map(), Map(), Map(), false, true, false),
      new ProcessedMetadata(Map(), Map(), Map(), false, true, false),
      new ProcessedMetadata(Map(), Map(), Map(), false, true, false),
    )
    val actual = returnImplementation.getTweetMetadataCounts(inputStream).map(_.photoUrlCount).compile.toVector
      .unsafeRunSync()
      .toList

    actual must beEqualTo(List(0,1,2,3))
  }

  private[this] def returnsEmojiTweetCount(): MatchResult[List[Int]] = {
    val inputStream = Stream(
      new ProcessedMetadata(Map(), Map(), Map(), false, false, false),
      new ProcessedMetadata(Map(), Map(), Map(), false, false, true),
      new ProcessedMetadata(Map(), Map(), Map(), false, false, true),
      new ProcessedMetadata(Map(), Map(), Map(), false, false, true),
      new ProcessedMetadata(Map(), Map(), Map(), false, false, false),
      new ProcessedMetadata(Map(), Map(), Map(), false, false, false)
    )

    val actual = returnImplementation.getTweetMetadataCounts(inputStream).map(_.emojiContainingCount).compile.toVector
      .unsafeRunSync()
      .toList

    actual must beEqualTo(List(0,1,2,3,3,3))
  }
}
