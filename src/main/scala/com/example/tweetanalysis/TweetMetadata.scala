package com.example.tweetanalysis

case class Metadata(hashtags: List[Hashtag])
object TweetMetadata {
  def get(t: Tweet): Metadata = {
    Metadata(t.entities.hashtags)
  }
}
