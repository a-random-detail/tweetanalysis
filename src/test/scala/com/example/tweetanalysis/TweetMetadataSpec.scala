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
  }

  private[this] def returnsCorrectHashtagList(): MatchResult[List[Hashtag]] = {
    val expectedHashtags = List[Hashtag](
      new Hashtag("first hashtag"),
      new Hashtag("second hashtag"),
      new Hashtag("third hashtag")
    )

    val inputTweet =
      Tweet("1970-01-01", "text here", new Entities(expectedHashtags))
    TweetMetadata.get(inputTweet).hashtags must beEqualTo(expectedHashtags)
  }

  private[this] def returnsEmptyListWhenNil(): MatchResult[List[Hashtag]] = {
    val inputTweet = Tweet("1970-01-01", "text here", new Entities(List()))
    TweetMetadata.get(inputTweet).hashtags must beEqualTo(List[Hashtag]())
  }
}
