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
      returnsUrlTrueWhenUrlPresent()
    }
    "returns empty list of urls whenn urls are missing" >> {
      returnsUrlFalseWhenNoUrlsPresent()
    }
  }

  private[this] def returnsCorrectHashtagList(): MatchResult[List[Hashtag]] = {
    val expectedHashtags = List[Hashtag](
      new Hashtag("first hashtag"),
      new Hashtag("second hashtag"),
      new Hashtag("third hashtag")
    )

    val inputTweet =
      Tweet("1970-01-01", "text here", new Entities(expectedHashtags, List()))
    TweetMetadata.get(inputTweet).hashtags must beEqualTo(expectedHashtags)
  }

  private[this] def returnsEmptyListWhenNil(): MatchResult[List[Hashtag]] = {
    val inputTweet = Tweet("1970-01-01", "text here", new Entities(List(), List()))
    TweetMetadata.get(inputTweet).hashtags must beEqualTo(List[Hashtag]())
  }

  private[this] def returnsUrlTrueWhenUrlPresent(): MatchResult[List[TweetUrl]] = {
      val expected = List(new TweetUrl("boom.com"), new TweetUrl("boom.org"))
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(List(), expected))
      TweetMetadata.get(inputTweet).urls must beEqualTo(expected)
  }
  private[this] def returnsUrlFalseWhenNoUrlsPresent(): MatchResult[List[TweetUrl]] = {
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(List(), List()))
      TweetMetadata.get(inputTweet).urls must beEqualTo(List())
  }
}
