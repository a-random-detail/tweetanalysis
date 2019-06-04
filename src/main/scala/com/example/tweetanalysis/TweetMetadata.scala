package com.example.tweetanalysis

case class Metadata(hashtags: List[Hashtag], urls: List[TweetUrl], photoUrls: List[MediaUrl])
object TweetMetadata {
  def get(t: Tweet): Metadata = {
    Metadata(t.entities.hashtags, t.entities.urls, t.entities.media.filter(_.media_type == "photo"))
  }
}
