package com.example.tweetanalysis

import annotation.tailrec
import cats.effect.Sync
import cats.implicits._

trait EmojiExtractor[F[_]] {
  def extractEmojis(s: String): List[String]
}

object EmojiExtractor {
  private val pattern = """(?:[\uD83C\uDF00-\uD83D\uDDFF]|[\uD83E\uDD00-\uD83E\uDDFF]|[\uD83D\uDE00-\uD83D\uDE4F]|[\uD83D\uDE80-\uD83D\uDEFF]|[\u2600-\u26FF]\uFE0F?|[\u2700-\u27BF]\uFE0F?|\u24C2\uFE0F?|[\uD83C\uDDE6-\uD83C\uDDFF]{1,2}|[\uD83C\uDD70\uD83C\uDD71\uD83C\uDD7E\uD83C\uDD7F\uD83C\uDD8E\uD83C\uDD91-\uD83C\uDD9A]\uFE0F?|[\u0023\u002A\u0030-\u0039]\uFE0F?\u20E3|[\u2194-\u2199\u21A9-\u21AA]\uFE0F?|[\u2B05-\u2B07\u2B1B\u2B1C\u2B50\u2B55]\uFE0F?|[\u2934\u2935]\uFE0F?|[\u3030\u303D]\uFE0F?|[\u3297\u3299]\uFE0F?|[\uD83C\uDE01\uD83C\uDE02\uD83C\uDE1A\uD83C\uDE2F\uD83C\uDE32-\uD83C\uDE3A\uD83C\uDE50\uD83C\uDE51]\uFE0F?|[\u203C\u2049]\uFE0F?|[\u25AA\u25AB\u25B6\u25C0\u25FB-\u25FE]\uFE0F?|[\u00A9\u00AE]\uFE0F?|[\u2122\u2139]\uFE0F?|\uD83C\uDC04\uFE0F?|\uD83C\uDCCF\uFE0F?|[\u231A\u231B\u2328\u23CF\u23E9-\u23F3\u23F8-\u23FA]\uFE0F?)"""

  def impl[F[_]: Sync]: EmojiExtractor[F] = new EmojiExtractor[F] {
    def extractEmojis(s: String): List[String] = extract(s, pattern.r.findAllIn(s).toList)

    @tailrec
    private def extract(text: String, emojiList: List[String], extracted: List[String] = List()): List[String] = (text, emojiList.scan("")(_ + _).drop(1)) match {
        case ("", _) => extracted
        case (_, List()) => extracted
        case (t, x :: xs) => {
          val syncedText = syncTextWithFirstEmoji(t, x)
          val nextEmoji = composeNextEmoji(syncedText, x :: xs)
          val remainingEmojis = emojiList.dropWhile(nextEmoji.contains(_))

          extract(text.diff(nextEmoji), remainingEmojis, extracted ::: List(nextEmoji))
        }
    }

    @tailrec
    def syncTextWithFirstEmoji(text: String, firstEmoji: String): String = (text, firstEmoji) match {
      case (s, prefix) if !s.startsWith(prefix) => syncTextWithFirstEmoji(s.substring(1), prefix)
      case _ => text
    }


    @tailrec
    def composeNextEmoji(text: String, emojiPrefixes: List[String], common: String = ""): String = (text, emojiPrefixes) match {
      case ("", _) => common
      case (_, List()) => common
      case (t, x :: xs) => composeNextEmoji(t, xs, x)
    }

  }



}
