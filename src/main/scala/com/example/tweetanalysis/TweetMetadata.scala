package com.example.tweetanalysis

import java.net.URL
import cats.effect.Sync
import cats.implicits._

trait TweetMetadata[F[_]] {
  def get(t: Tweet): Metadata
}

case class Metadata(hashtags: List[String], domains: List[String], urls: List[TweetUrl], photoUrls: List[MediaUrl], emojiList: List[String])
object TweetMetadata {
  def impl[F[_]: Sync]: TweetMetadata[F] = new TweetMetadata[F] {
    val extractor = EmojiExtractor.impl[F]
    def get(t: Tweet): Metadata = {
      val urlList = t.entities.urls.getOrElse(List())
      val extracted = extractor.extractEmojis(t.text).toList

      Metadata(t.entities.hashtags.getOrElse(List()).map(_.text), urlList.map(getDomainFromUrl(_)).distinct, urlList, t.entities.media.getOrElse(List()).filter(x => x.media_type == "photo" && x.indices.length > 0), extracted)
    }

    private def getDomainFromUrl(s: TweetUrl): String = new URL(s.expanded_url).getHost
}


}
