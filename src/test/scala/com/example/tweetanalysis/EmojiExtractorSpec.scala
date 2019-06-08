package com.example.tweetanalysis

import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import org.specs2.matcher.MatchResult
import fs2.Stream
import java.time._

class EmojiExtractorSpec extends org.specs2.mutable.Specification {

  "EmojiExtractor" >> {
    "returns emojis as list" >> {
      returnsListOfEmojis()
    }
    "returns empty list when no emojis found" >> {
      returnsEmplyListForNoEmojis()
    }
    "does not decompose complex emojis - fitzpatrick modifier" >> {
      fitzpatrickModifierIsNotSplitOut()
    }
  }

  private[this] def returnImplementation: EmojiExtractor[IO] = EmojiExtractor.impl[IO]

  private[this] def returnsListOfEmojis(): MatchResult[List[String]] = {
    val inputText = "tweet😘 with an emoji "
    returnImplementation.extractEmojis(inputText) must beEqualTo(List("😘"))
  }

  private[this] def returnsEmplyListForNoEmojis(): MatchResult[List[String]] = {
    val inputText = "tweet without an emoji"
    returnImplementation.extractEmojis(inputText) must beEqualTo(List())
  }

  private[this] def fitzpatrickModifierIsNotSplitOut(): MatchResult[List[String]] = {
    val inputText = "check out this emoji with a fitzpatrick modifier👌🏿"
    returnImplementation.extractEmojis(inputText) must beEqualTo(List("👌🏿"))
  }

}
