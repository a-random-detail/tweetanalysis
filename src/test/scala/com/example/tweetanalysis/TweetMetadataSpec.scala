package com.example.tweetanalysis

import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import org.specs2.matcher.MatchResult
import fs2.Stream

class TweetMetadataSpec extends org.specs2.mutable.Specification {

  "TweetMetadata" >> {
    "returns correct hashtag list" >> {
      returnsCorrectHashtagList()
    }
    "handles empty hashtags" >> {
      returnsEmptyListWhenNil()
    }
    "returns list of urls when urls are present" >> {
      returnsUrlTrueWhenUrlsPresent()
    }
    "returns empty list of urls whenn urls are missing" >> {
      returnsUrlFalseWhenNoUrlsPresent()
    }
    "returns list of media urls when present" >> {
      returnsUrlTrueWhenMediaUrlsPresent()
    }
    "returns empty list of media urls when media urls are missing" >> {
      returnsUrlFalseWhenNoMediaUrlsPresent()
    }
    "ignores non-photo media urls" >> {
      ignoresNonPhotoMediaUrls()
    }
  }

  private[this] def returnsCorrectHashtagList(): MatchResult[List[Hashtag]] = {
    val expectedHashtags = List[Hashtag](
      new Hashtag("first hashtag"),
      new Hashtag("second hashtag"),
      new Hashtag("third hashtag")
    )

    val inputTweet =
      Tweet("1970-01-01", "text here", new Entities(Some(expectedHashtags), Some(List()), Some(List())))
    TweetMetadata.get(inputTweet).hashtags must beEqualTo(expectedHashtags)
  }

  private[this] def returnsEmptyListWhenNil(): MatchResult[List[Hashtag]] = {
    val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(List()), Some(List())))
    TweetMetadata.get(inputTweet).hashtags must beEqualTo(List[Hashtag]())
  }

  private[this] def returnsUrlTrueWhenUrlsPresent(): MatchResult[List[TweetUrl]] = {
      val expected = List(new TweetUrl("boom.com"), new TweetUrl("boom.org"))
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(expected), Some(List())))
      TweetMetadata.get(inputTweet).urls must beEqualTo(expected)
  }

  private[this] def returnsUrlFalseWhenNoUrlsPresent(): MatchResult[List[TweetUrl]] = {
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(List()), Some(List())))
      TweetMetadata.get(inputTweet).urls must beEqualTo(List())
  }

  private[this] def returnsUrlTrueWhenMediaUrlsPresent(): MatchResult[List[MediaUrl]] = {
      val expected = List(new MediaUrl("boom.com", "photo", List(2,14)), new MediaUrl("boom.org","photo", List(12, 24)))
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()),Some(List()),Some(expected)))
      TweetMetadata.get(inputTweet).photoUrls must beEqualTo(expected)
  }

  private[this] def returnsUrlFalseWhenNoMediaUrlsPresent(): MatchResult[List[MediaUrl]] = {
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(List()), Some(List())))
      TweetMetadata.get(inputTweet).photoUrls must beEqualTo(List())
  }

  private[this] def ignoresNonPhotoMediaUrls(): MatchResult[List[MediaUrl]] = {
    val shouldNotShowUp = List(new MediaUrl("do not see this", "video", List(2,22)), new MediaUrl("do not see this either", "animated_gif", List(1, 34)))
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(List()), Some(shouldNotShowUp)))
      TweetMetadata.get(inputTweet).photoUrls must beEqualTo(List())
  }

  private[this] def ignoresMediaUrlsWithoutIndices(): MatchResult[List[MediaUrl]] = {
      val shouldNotShowUp = List(new MediaUrl("do not see this", "video", List()), new MediaUrl("do not see this either", "animated_gif", List()))
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(List()), Some(shouldNotShowUp)))
      TweetMetadata.get(inputTweet).photoUrls must beEqualTo(List())
  }
}
