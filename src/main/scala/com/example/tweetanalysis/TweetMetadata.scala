package com.example.tweetanalysis

case class Metadata(hashtags: List[Hashtag], urls: List[TweetUrl])
object TweetMetadata {
  def get(t: Tweet): Metadata = {
    Metadata(t.entities.hashtags, t.entities.urls)
  }
}
