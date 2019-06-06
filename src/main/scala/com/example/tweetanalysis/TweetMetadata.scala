package com.example.tweetanalysis

import java.net.URL

case class Metadata(hashtags: List[String], domains: List[String], urls: List[TweetUrl], photoUrls: List[MediaUrl])
object TweetMetadata {
  def get(t: Tweet): Metadata = {
    val urlList = t.entities.urls.getOrElse(List())
    Metadata(t.entities.hashtags.getOrElse(List()).map(_.text), urlList.map(getDomainFromUrl(_)).distinct, urlList, t.entities.media.getOrElse(List()).filter(x => x.media_type == "photo" && x.indices.length > 0))
  }

  private def getDomainFromUrl(s: TweetUrl): String = new URL(s.expanded_url).getHost

}
