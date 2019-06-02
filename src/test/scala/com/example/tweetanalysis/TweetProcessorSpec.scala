package com.example.tweetanalysis

import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import org.specs2.matcher.MatchResult
import fs2.Stream

class TweetProcessorSpec extends org.specs2.mutable.Specification {

  "TweetProcessor" >> {
    "returns stream containing total tweet counts" >> {
      returnsCorrectStreamMappingForTotalTweets()
    }
  }

  private[this] def returnProcessingResult(input: Stream[IO, Tweet]): Stream[IO, AnalysisResult] = TweetProcessor.impl[IO].analyze(input)

  private[this] def returnsCorrectStreamMappingForTotalTweets(): MatchResult[Boolean] = {
    val input = Stream(
      new Tweet("1970-01-01", "First test tweet",  new Entities(List[Hashtag]())),
      new Tweet("1970-02-07", "Second test tweet",  new Entities(List[Hashtag]())),
      new Tweet("1970-03-06", "Third test tweet",  new Entities(List[Hashtag]())),
      new Tweet("1970-01-02", "Fourth test tweet",  new Entities(List[Hashtag]())),
      new Tweet("1970-02-14", "Fifth test tweet",  new Entities(List[Hashtag]())),
      new Tweet("1970-03-28", "Sixth test tweet",  new Entities(List[Hashtag]()))
    )
    val expectedOutput = Stream(1,2,3,4,5,6).map(AnalysisResult(_)).toList
    returnProcessingResult(input).compile.toVector.unsafeRunSync().toList.forall(expectedOutput.contains(_)) must beTrue
  }
}
