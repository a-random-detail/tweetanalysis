package com.example.tweetanalysis

case class Metadata(hashtags: List[Hashtag], urls: List[TweetUrl], photoUrls: List[MediaUrl])
object TweetMetadata {
  def get(t: Tweet): Metadata = {
    Metadata(t.entities.hashtags.getOrElse(List()), t.entities.urls.getOrElse(List()), t.entities.media.getOrElse(List()).filter(x => x.media_type == "photo" && x.indices.length > 0))
  }

}
