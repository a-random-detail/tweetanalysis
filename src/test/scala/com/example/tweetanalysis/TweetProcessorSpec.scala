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
  }

  private[this] def returnProcessingResult(
      input: Stream[IO, Tweet]): Stream[IO, AnalysisResult] =
    TweetProcessor.impl[IO].analyze(input)

  private[this] def returnsCorrectStreamMappingForTotalTweets()
    : MatchResult[Boolean] = {
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(List[Hashtag]()))
    )
    val expectedOutput = Stream(1, 2, 3, 4, 5, 6)
      .map(AnalysisResult(_, 0.0, 0.0, 0.0, 0.0, Map[Hashtag, Int]()))
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
                new Entities(List[Hashtag]())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(List[Hashtag]()))
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
                new Entities(List[Hashtag]())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(List[Hashtag]()))
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
                new Entities(List[Hashtag]())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(List[Hashtag]()))
    )
    val result =
      returnProcessingResult(input).compile.toVector.unsafeRunSync().toList
    result.forall(x => (x.tweetsPerSecond * 60) == x.tweetsPerMinute) must beTrue
  }

  private[this] def returnsTweetsPerHour(): MatchResult[Boolean] = {
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(List[Hashtag]()))
    )
    val result = returnProcessingResult(input).compile.toVector
      .unsafeRunSync()
      .toList
      .map(_.tweetsPerHour)
    validTimeSeries(result) must beTrue
  }

  private[this] def returnsTweetsPerHourBasedOnSeconds()
    : MatchResult[Boolean] = {
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-01-02",
                "Fourth test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-02-14",
                "Fifth test tweet",
                new Entities(List[Hashtag]())),
      new Tweet("1970-03-28",
                "Sixth test tweet",
                new Entities(List[Hashtag]()))
    )
    val result =
      returnProcessingResult(input).compile.toVector.unsafeRunSync().toList
    result.forall(x => (x.tweetsPerSecond * 60 * 60) == x.tweetsPerHour) must beTrue
  }

  private[this] def returnsTopHashtags(): MatchResult[Boolean] = {
    val topHashtag = Hashtag("blerp")
    val secondHashtag = Hashtag("bleep")
    val thirdHashtag = Hashtag("ayoo")
    val fourthHashtag = Hashtag("ah")
    val fifthHashtag = Hashtag("no")
    val input = Stream(
      new Tweet("1970-01-01",
                "First test tweet",
                new Entities(List[Hashtag](topHashtag, thirdHashtag))),
      new Tweet("1970-02-07",
                "Second test tweet",
                new Entities(List[Hashtag](topHashtag, secondHashtag))),
      new Tweet("1970-03-06",
                "Third test tweet",
                new Entities(List[Hashtag](topHashtag, fourthHashtag))),
      new Tweet("1970-03-08",
                "Fourth test tweet",
                new Entities(List[Hashtag](topHashtag, secondHashtag))),
      new Tweet("1970-03-09",
                "Fifth test tweet",
                new Entities(List[Hashtag](thirdHashtag, secondHashtag))),
      new Tweet("1970-03-09",
                "Sixth test tweet",
                new Entities(List[Hashtag](thirdHashtag, secondHashtag)))
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
    val resultMatchesExpected = (0 to 4)
      .map(x => result(x).equals(expectedTopHashtagsList(x)))
      .forall(_ == true)
    resultMatchesExpected must beTrue
  }

  private[this] def validTimeSeries(l: List[Double]): Boolean =
    (l == l.sorted) && l.forall(_ > 0)

}
