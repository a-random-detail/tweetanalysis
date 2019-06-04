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
  }

  private[this] def returnProcessingResult(
      input: Stream[IO, Tweet]): Stream[IO, AnalysisResult] =
    TweetProcessor.impl[IO].analyze(input)

  private[this] def returnsCorrectStreamMappingForTotalTweets()
    : MatchResult[Boolean] = {
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(List(),List(), List()))
    )
    val expectedOutput = Stream(1, 2, 3, 4, 5, 6)
      .map(AnalysisResult(_, 0.0, 0.0, 0.0, 0.0, Map[Hashtag, Int](), 0.0, 0.0))
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
                new Entities(List(),List(), List())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(List(),List(), List()))
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
                new Entities(List(),List(), List())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(List(),List(), List()))
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
                new Entities(List(),List(), List())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(List(),List(), List()))
    )
    val result =
      returnProcessingResult(input).compile.toVector.unsafeRunSync().toList
    result.forall(x => (x.tweetsPerSecond * 60) == x.tweetsPerMinute) must beTrue
  }

  private[this] def returnsTweetsPerHour(): MatchResult[Boolean] = {
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(List(),List(), List())),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(List(),List(), List()))
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
                new Entities(List(), List(), List())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List(), List(), List())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List(), List(), List())),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(List(), List(), List())),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(List(), List(), List())),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(List(), List(), List()))
    )
    val result =
      returnProcessingResult(input).compile.toVector.unsafeRunSync().toList
    val expected = result.map(_.tweetsPerSecond * 60 * 60)
    val actual = result.map(_.tweetsPerHour)
    actual must beEqualTo(expected)
  }

  private[this] def returnsTopHashtags(): MatchResult[List[Map[Hashtag, Int]]] = {
    val topHashtag = Hashtag("blerp")
    val secondHashtag = Hashtag("bleep")
    val thirdHashtag = Hashtag("ayoo")
    val fourthHashtag = Hashtag("ah")
    val fifthHashtag = Hashtag("no")
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(List[Hashtag](topHashtag, thirdHashtag), List(), List())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List[Hashtag](topHashtag, secondHashtag), List(), List())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List[Hashtag](topHashtag, fourthHashtag), List(), List())),
      new Tweet("1970-03-08",
                "Fourth test tweet",
                new Entities(List[Hashtag](secondHashtag, topHashtag), List(), List())),
      new Tweet("1970-03-09",
                "Fifth test tweet",
                new Entities(List[Hashtag](thirdHashtag, secondHashtag), List(), List()))
    )
    val expectedTopHashtagsList = List(
      Map(topHashtag -> 1, thirdHashtag -> 1),
      Map(topHashtag -> 2, thirdHashtag -> 1, secondHashtag -> 1),
      Map(topHashtag -> 3, fourthHashtag -> 1, thirdHashtag -> 1),
      Map(topHashtag -> 4, secondHashtag -> 2, fourthHashtag -> 1),
      Map(topHashtag -> 4, secondHashtag -> 3, thirdHashtag -> 2)
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
                new Entities(List(), List(new TweetUrl("http://example.com")), List())),
      new Tweet("1970-4-10", "test tweet without url", new Entities(List(), List(), List())),
      new Tweet("1970-4-10", "test tweet without url #2", new Entities(List(), List(), List())),
      new Tweet("1970-4-10", "test tweet without url #3", new Entities(List(), List(), List())),
      new Tweet("1970-4-10", "test tweet with url #2", new Entities(List(), List(new TweetUrl("http://example2.com")), List()))
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
                new Entities(List(), List(), List(new MediaUrl("https://pictureshere.com")))),
      new Tweet("1970-4-10", "test tweet without url", new Entities(List(), List(), List())),
      new Tweet("1970-4-10", "test tweet without url #2", new Entities(List(), List(), List(new MediaUrl("http://also-pictures.net")))),
      new Tweet("1970-4-10", "test tweet without url #3", new Entities(List(), List(), List())),
      new Tweet("1970-4-10", "test tweet with url #2", new Entities(List(), List(), List(new MediaUrl("http://example2.com"))))
    )
    val expectedPercentages = List(100.00, 50.00, 66.67, 50.00, 60.00)
    val result = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(_.percentageContainingPhotoUrl)

      result must beEqualTo(expectedPercentages)
  }

  private[this] def validTimeSeries(l: List[Double]): Boolean =
    (l == l.sorted) && l.forall(_ > 0)

}
