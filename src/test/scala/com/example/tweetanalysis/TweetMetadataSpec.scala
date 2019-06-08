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
      returnsEmptyListWhenHashtagsEmpty()
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
    "returns correct domain list" >> {
      returnsCorrectDomainList()
    }
    "handles empty domains" >> {
      returnsEmptyListWhenUrlListEmpty()
    }
    "removes duplicate domains" >> {
      removesDomainDuplicates()
    }
    "returns list of distinct emojis" >> {
      returnsListOfDistinctEmojisInText()
    }
    "returns no emojis when they are not in tweet text" >> {
      returnsNoEmojisWhenNoneAreInTweetText()
    }
  }

  private[this] def returnsImplementation: TweetMetadata[IO] = TweetMetadata.impl[IO]

  private[this] def returnsCorrectHashtagList(): MatchResult[List[String]] = {
    val inputHashtags = List[Hashtag](
      new Hashtag("first hashtag"),
      new Hashtag("second hashtag"),
      new Hashtag("third hashtag")
    )
    val expected = inputHashtags.map(_.text)

    val inputTweet =
      Tweet("1970-01-01", "text here", new Entities(Some(inputHashtags), Some(List()), Some(List())))
    returnsImplementation.get(inputTweet).hashtags must beEqualTo(expected)
  }

  private[this] def returnsEmptyListWhenHashtagsEmpty(): MatchResult[List[String]] = {
    val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(List()), Some(List())))
    returnsImplementation.get(inputTweet).hashtags must beEqualTo(List[String]())
  }

  private[this] def returnsUrlTrueWhenUrlsPresent(): MatchResult[List[TweetUrl]] = {
      val expected = List(new TweetUrl("boom.com", "http://unwoundurl.com"), new TweetUrl("boom.org", "http://boomboom.org"))
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(expected), Some(List())))
      returnsImplementation.get(inputTweet).urls must beEqualTo(expected)
  }

  private[this] def returnsUrlFalseWhenNoUrlsPresent(): MatchResult[List[TweetUrl]] = {
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(List()), Some(List())))
      returnsImplementation.get(inputTweet).urls must beEqualTo(List())
  }

  private[this] def returnsUrlTrueWhenMediaUrlsPresent(): MatchResult[List[MediaUrl]] = {
      val expected = List(new MediaUrl("boom.com", "photo", List(2,14)), new MediaUrl("boom.org","photo", List(12, 24)))
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()),Some(List()),Some(expected)))
      returnsImplementation.get(inputTweet).photoUrls must beEqualTo(expected)
  }

  private[this] def returnsUrlFalseWhenNoMediaUrlsPresent(): MatchResult[List[MediaUrl]] = {
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(List()), Some(List())))
      returnsImplementation.get(inputTweet).photoUrls must beEqualTo(List())
  }

  private[this] def ignoresNonPhotoMediaUrls(): MatchResult[List[MediaUrl]] = {
    val shouldNotShowUp = List(new MediaUrl("do not see this", "video", List(2,22)), new MediaUrl("do not see this either", "animated_gif", List(1, 34)))
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(List()), Some(shouldNotShowUp)))
      returnsImplementation.get(inputTweet).photoUrls must beEqualTo(List())
  }

  private[this] def ignoresMediaUrlsWithoutIndices(): MatchResult[List[MediaUrl]] = {
      val shouldNotShowUp = List(new MediaUrl("do not see this", "video", List()), new MediaUrl("do not see this either", "animated_gif", List()))
      val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(List()), Some(shouldNotShowUp)))
      returnsImplementation.get(inputTweet).photoUrls must beEqualTo(List())
  }

  private[this] def handlesNoneValues(): MatchResult[Metadata] = {
    val expected = Metadata(List(), List(), List(), List(), List())
    val inputTweet = Tweet("1970-01-01", "text here", new Entities(None, None, None))
    returnsImplementation.get(inputTweet) must beEqualTo(expected)
  }

  private[this] def returnsCorrectDomainList(): MatchResult[List[String]] = {
    val inputUrls = List(
      new TweetUrl("https://t.co/asdfa", "https://firstdomain.org"),
      new TweetUrl("https://t.co/aaaaa", "https://seconddomain.com")
    )

    val expected = List(
        "firstdomain.org",
        "seconddomain.com"
    )

    val inputTweet =
      Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(inputUrls), Some(List())))
    returnsImplementation.get(inputTweet).domains must beEqualTo(expected)
  }

  private[this] def removesDomainDuplicates(): MatchResult[List[String]] = {
    val inputUrls = List(
      new TweetUrl("https://t.co/bbbbb", "https://firstdomain.org"),
      new TweetUrl("https://t.co/asdfa", "https://seconddomain.com"),
      new TweetUrl("https://t.co/aaaaa", "https://seconddomain.com")
    )

    val expected = List(
        "firstdomain.org",
        "seconddomain.com"
    )

    val inputTweet =
      Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(inputUrls), Some(List())))
    returnsImplementation.get(inputTweet).domains must beEqualTo(expected)
  }

  private[this] def returnsEmptyListWhenUrlListEmpty(): MatchResult[List[String]] = {
    val inputTweet = Tweet("1970-01-01", "text here", new Entities(Some(List()), Some(List()), Some(List())))
    returnsImplementation.get(inputTweet).domains must beEqualTo(List[String]())
  }

  private[this] def returnsListOfDistinctEmojisInText(): MatchResult[List[String]] = {
    val expected = List(
      "👨🏿‍🏫",
      "👱🏽",
      "👨🏼‍🍳",
      "☔️",
      "☂️",
      "🤯"
    )
    val inputTweet = Tweet("1970-01-01", "👨🏿text👱🏽here👨🏼‍🍳with☔️emoji☂️🤯🤯🤯 🤯 🤯 🤯", new Entities(None, None, None))
    returnsImplementation.get(inputTweet).emojiList must beEqualTo(expected)
  }

  private[this] def returnsNoEmojisWhenNoneAreInTweetText(): MatchResult[List[String]] = {
    val inputTweet = Tweet("1970-01-01", "text here without emoji", new Entities(None, None, None))
    returnsImplementation.get(inputTweet).emojiList must beEqualTo(List[String]())
  }
}
