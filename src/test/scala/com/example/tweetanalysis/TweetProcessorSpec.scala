package com.example.tweetanalysis

import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import org.specs2.matcher.MatchResult
import fs2.Stream
import java.time._

class TweetProcessorSpec extends org.specs2.mutable.Specification {

  "TweetProcessor" >> {
    "returns stream containing total tweet counts" >> {
      returnsCorrectStreamMappingForTotalTweets()
    }
    "returns correct average tweets per second" >> {
      returnsCorrectTweetsPerSecond()
    }
    "returns average tweets per minute" >> {
      returnsTweetsPerMinute()
    }
    "returns tweetsPerMinute proportional to tweets per second" >> {
      returnsTweetsPerMinuteBasedOnSeconds()
    }
    "returns average tweets per hour" >> {
      returnsTweetsPerHour()
    }
    "returns tweetsPerHour proportional to tweets per second" >> {
      returnsTweetsPerHourBasedOnSeconds()
    }
    "returns top 3 hashtags sorted alphabetically for ties" >> {
      returnsTopHashtags()
    }
    "returns percent of tweets that contain a url" >> {
      returnsTweetsContainingUrlPercentages()
    }
    "returns percent of tweets that contain photo urls" >> {
      returnsPercentageOfTweetsContainingPhotoUrl()
    }
    "ignores non-photo media urls when accounting for percentage of tweets with photos" >> {
      ignoresNonPhotoMediaUrls()
    }
    "ignores photo media urls without indices" >> {
      ignoresMediaUrlsWithoutIndices()
    }
    "handles entities fields with a value of None" >> {
      canHandleNoneEntitiesFields()
    }

    "returns top 3 url domains sorted alphabetically for ties" >> {
      returnsTopUrlDomains()
    }
  }

  private[this] def returnProcessingResult(
      input: Stream[IO, Tweet]): Stream[IO, AnalysisResult] =
    TweetProcessor.impl[IO].analyze(input)

  private[this] def returnsCorrectStreamMappingForTotalTweets()
    : MatchResult[Boolean] = {
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(Some(List()),Some(List()), Some(List())))
    )
    val expectedOutput = Stream(1, 2, 3, 4, 5, 6)
      .map(AnalysisResult(_, 0.0, 0.0, 0.0, 0.0, Map[String, Int](), Map[String, Int](), 0.0, 0.0))
      .toList
      .map(x => x.totalTweets)
    val result = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(x => x.totalTweets)
    result.forall(expectedOutput.contains(_)) must beTrue
  }

  private[this] def returnsCorrectTweetsPerSecond(): MatchResult[Boolean] = {
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(Some(List()),Some(List()), Some(List())))
    )
    val result = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(_.tweetsPerSecond)
    validTimeSeries(result) must beTrue
  }

  private[this] def returnsTweetsPerMinute(): MatchResult[Boolean] = {
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(Some(List()),Some(List()), Some(List())))
    )
    val result = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(_.tweetsPerMinute)
    validTimeSeries(result) must beTrue
  }

  private[this] def returnsTweetsPerMinuteBasedOnSeconds()
    : MatchResult[Boolean] = {
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(Some(List()),Some(List()), Some(List())))
    )
    val result =
      returnProcessingResult(input).compile.toVector.unsafeRunSync().toList
    result.forall(x => (x.tweetsPerSecond * 60) == x.tweetsPerMinute) must beTrue
  }

  private[this] def returnsTweetsPerHour(): MatchResult[Boolean] = {
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(Some(List()),Some(List()), Some(List()))),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(Some(List()),Some(List()), Some(List())))
    )
    val result = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(_.tweetsPerHour)
    validTimeSeries(result) must beTrue
  }

  private[this] def returnsTweetsPerHourBasedOnSeconds()
    : MatchResult[List[Double]] = {
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(Some(List()), Some(List()), Some(List()))),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(Some(List()), Some(List()), Some(List()))),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(Some(List()), Some(List()), Some(List()))),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(Some(List()), Some(List()), Some(List()))),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(Some(List()), Some(List()), Some(List()))),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(Some(List()), Some(List()), Some(List())))
    )
    val result =
      returnProcessingResult(input).compile.toVector.unsafeRunSync().toList
    val expected = result.map(_.tweetsPerSecond * 60 * 60)
    val actual = result.map(_.tweetsPerHour)
    actual must beEqualTo(expected)
  }

  private[this] def returnsTopHashtags(): MatchResult[List[Map[String, Int]]] = {
    val topHashtag = Hashtag("blerp")
    val secondHashtag = Hashtag("bleep")
    val thirdHashtag = Hashtag("ayoo")
    val fourthHashtag = Hashtag("ah")
    val fifthHashtag = Hashtag("no")
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(Some(List(topHashtag, thirdHashtag)), Some(List()), Some(List()))),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(Some(List(topHashtag, secondHashtag)), Some(List()), Some(List()))),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(Some(List(topHashtag, fourthHashtag)), Some(List()), Some(List()))),
      new Tweet("1970-03-08",
                "Fourth test tweet",
                new Entities(Some(List(secondHashtag, topHashtag)), Some(List()), Some(List()))),
      new Tweet("1970-03-09",
                "Fifth test tweet",
                new Entities(Some(List(thirdHashtag, secondHashtag)), Some(List()), Some(List())))
    )
    val expectedTopHashtagsList = List(
      Map(topHashtag.text -> 1, thirdHashtag.text -> 1),
      Map(topHashtag.text -> 2, thirdHashtag.text -> 1, secondHashtag.text -> 1),
      Map(topHashtag.text -> 3, fourthHashtag.text -> 1, thirdHashtag.text -> 1),
      Map(topHashtag.text -> 4, secondHashtag.text -> 2, fourthHashtag.text -> 1),
      Map(topHashtag.text -> 4, secondHashtag.text -> 3, thirdHashtag.text -> 2)
    )
    val result = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(_.topHashtags)
    result must beEqualTo(expectedTopHashtagsList)
  }

  private[this] def returnsTweetsContainingUrlPercentages(): MatchResult[List[Double]] = {
    val input = Stream(
      new Tweet("1970-03-09",
                "test tweet with url",
                new Entities(Some(List()), Some(List(new TweetUrl("http://example.com", "http://unwoundurl.com"))), Some(List()))),
      new Tweet("1970-4-10", "test tweet without url", new Entities(Some(List()), Some(List()), Some(List()))),
      new Tweet("1970-4-10", "test tweet without url #2", new Entities(Some(List()), Some(List()), Some(List()))),
      new Tweet("1970-4-10", "test tweet without url #3", new Entities(Some(List()), Some(List()), Some(List()))),
      new Tweet("1970-4-10", "test tweet with url #2", new Entities(Some(List()), Some(List(new TweetUrl("http://example2.com", "http://unwoundurl2.com"))), Some(List())))
    )
    val expectedPercentages = List(100.00, 50.00, 33.33, 25.00, 40.00)
    val result = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(_.percentageContainingUrl)

      result must beEqualTo(expectedPercentages)
  }

  private[this] def returnsPercentageOfTweetsContainingPhotoUrl(): MatchResult[List[Double]] = {
    val input = Stream(
      new Tweet("1970-03-09",
                "test tweet with url",
                new Entities(Some(List()), Some(List()), Some(List(new MediaUrl("https://pictureshere.com", "photo",List(12, 24)))))),
      new Tweet("1970-4-10", "test tweet without url", new Entities(Some(List()),Some(List()),Some(List()))),
      new Tweet("1970-4-10", "test tweet without url #2", new Entities(Some(List()), Some(List()), Some(List(new MediaUrl("http://also-pictures.net", "photo", List(21, 52)))))),
      new Tweet("1970-4-10", "test tweet without url #3", new Entities(Some(List()),Some(List()),Some(List()))),
      new Tweet("1970-4-10", "test tweet with url #2", new Entities(Some(List()), Some(List()), Some(List(new MediaUrl("http://example2.com", "photo", List(1,24))))))
    )
    val expectedPercentages = List(100.00, 50.00, 66.67, 50.00, 60.00)
    val result = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(_.percentageContainingPhotoUrl)

      result must beEqualTo(expectedPercentages)
  }

  private[this] def ignoresNonPhotoMediaUrls(): MatchResult[List[Double]] = {
    val input = Stream(
      new Tweet("1970-03-09",
                "test tweet with url",
                new Entities(Some(List()), Some(List()), Some(List(new MediaUrl("https://pictureshere.com", "photo", List(1,14)))))),
      new Tweet("1970-4-10", "test tweet without url", new Entities(Some(List()),Some(List()),Some(List()))),
      new Tweet("1970-4-10", "test tweet without url #2", new Entities(Some(List()),Some(List()), Some(List(new MediaUrl("http://also-pictures.net", "animated_gif", List(1,24)))))),
      new Tweet("1970-4-10", "test tweet without url #3", new Entities(Some(List()),Some(List()),Some(List()))),
      new Tweet("1970-4-10", "test tweet with url #2", new Entities(Some(List()),Some(List()), Some(List(new MediaUrl("http://example2.com", "video", List(1,24))))))
    )
    val expectedPercentages = List(100.00, 50.00, 33.33, 25.00, 20.00)
    val result = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(_.percentageContainingPhotoUrl)

      result must beEqualTo(expectedPercentages)
  }
  private[this] def ignoresMediaUrlsWithoutIndices(): MatchResult[List[Double]] = {
    val input = Stream(
      new Tweet("1970-03-09",
                "test tweet with url",
                new Entities(Some(List()), Some(List()), Some(List(new MediaUrl("https://pictureshere.com", "photo", List()))))),
      new Tweet("1970-4-10", "test tweet without url", new Entities(Some(List()),Some(List()),Some(List()))),
      new Tweet("1970-4-10", "test photo tweet without indices", new Entities(Some(List()), Some(List()), Some(List(new MediaUrl("https://pictureurlthatisignored.com", "photo", List(12,24)))))),
    )

    val expectedPercentages = List(0.0, 0.0, 33.33)
    val result = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(_.percentageContainingPhotoUrl)

      result must beEqualTo(expectedPercentages)
  }
  private[this] def canHandleNoneEntitiesFields(): MatchResult[(Map[String, Int], Double, Double)] = {
    val input = Stream(
      new Tweet("1970-03-09",
                "test tweet with url",
                new Entities(None, None, None))
    )

    val expected = (Map[String, Int](),0.0,0.0)
    val result: List[(Map[String, Int], Double, Double)] = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(x => (x.topHashtags, x.percentageContainingUrl, x.percentageContainingPhotoUrl))

      result.head must beEqualTo(expected)
  }

  private[this] def returnsTopUrlDomains(): MatchResult[List[Map[String, Int]]] = {

    val urlGroup1_1 = TweetUrl("https://t.co/ayoo", "https://twitter.com/boom/bang/134/pow/all")
    val urlGroup1_2 = TweetUrl("https://t.co/bleep", "https://twitter.com/bingbangboom")

    val urlGroup2_1 = TweetUrl("https://t.co/adsf", "https://boomboom.com/check_this_out.json")

    val urlGroup3_1 = TweetUrl("https://t.co/aaaa", "https://heyheyheyfatalbert.here/yep")

    val urlGroup4_1 = TweetUrl("https://t.co/hahaha", "https://fourthurl.com/more_laughing")

    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(Some(List()), Some(List(urlGroup1_1)), Some(List()))),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(Some(List()), Some(List(urlGroup2_1)), Some(List()))),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(Some(List()), Some(List(urlGroup3_1)), Some(List()))),
      new Tweet("1970-03-08",
                "Fourth test tweet",
                new Entities(Some(List()), Some(List(urlGroup1_2)), Some(List()))),
      new Tweet("1970-03-09",
                "Fifth test tweet",
                new Entities(Some(List()), Some(List(urlGroup4_1)), Some(List())))
    )
    val expected = List(
      Map(urlGroup1_1.expanded_url -> 1),
      Map(urlGroup2_1.expanded_url -> 1, urlGroup1_1.expanded_url -> 1),
      Map(urlGroup2_1.expanded_url -> 1, urlGroup3_1.expanded_url -> 1, urlGroup1_1.expanded_url -> 1),
      Map(urlGroup1_2.expanded_url -> 2, urlGroup2_1.expanded_url -> 1, urlGroup3_1.expanded_url -> 1),
      Map(urlGroup1_2.expanded_url -> 2, urlGroup2_1.expanded_url -> 1, urlGroup3_1.expanded_url -> 1)
    )
    val result = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(_.topDomains)
    result must beEqualTo(expected)
  }

  private[this] def validTimeSeries(l: List[Double]): Boolean =
    (l == l.sorted) && l.forall(_ > 0)

}
