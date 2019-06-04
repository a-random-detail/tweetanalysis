package com.example.tweetanalysis

import cats.effect.Sync
import cats.Applicative
import cats.implicits._
import io.circe._
import io.circe.generic.semiauto._
import org.http4s.circe._
import org.http4s.{EntityDecoder, EntityEncoder}


case class Tweet(created_at: String, text: String, entities: Entities)
object Tweet {
  implicit val tweetDecoder: Decoder[Tweet] = deriveDecoder[Tweet]
  implicit def tweetEntityDecoder[F[_]: Sync]: EntityDecoder[F, Tweet] =
    jsonOf
  implicit val tweetEncoder: Encoder[Tweet] = deriveEncoder[Tweet]
  implicit def tweetEntityEncoder[F[_]: Applicative]: EntityEncoder[F, Tweet] =
    jsonEncoderOf
}

case class Entities(hashtags: List[Hashtag], urls: List[TweetUrl], mediaUrls: List[MediaUrl])
object Entities {
  implicit val entityDecoder: Decoder[Entities] = deriveDecoder[Entities]
  implicit def entityEntityDecoder[F[_]: Sync]: EntityDecoder[F, Entities] =
    jsonOf
  implicit val entityEncoder: Encoder[Entities] = deriveEncoder[Entities]
  implicit def entityEntityEncoder[F[_]: Applicative]: EntityEncoder[F, Entities] =
    jsonEncoderOf
}

case class Hashtag(text: String)
object Hashtag {
  implicit val hashtagDecoder: Decoder[Hashtag] = deriveDecoder[Hashtag]
  implicit def hashtagEntityDecoder[F[_]: Sync]: EntityDecoder[F, Hashtag] =
    jsonOf
  implicit val hashtagEncoder: Encoder[Hashtag] = deriveEncoder[Hashtag]
  implicit def hashtagEntityEncoder[F[_]: Applicative]: EntityEncoder[F, Hashtag] =
    jsonEncoderOf
}

case class TweetUrl(url: String)
object TweetUrl {
  implicit val tweetUrlDecoder: Decoder[TweetUrl] = deriveDecoder[TweetUrl]
  implicit def tweetUrlEntityDecoder[F[_]: Sync]: EntityDecoder[F, TweetUrl] =
    jsonOf
  implicit val tweetUrlEncoder: Encoder[TweetUrl] = deriveEncoder[TweetUrl]
  implicit def tweetUrlEntityEncoder[F[_]: Applicative]: EntityEncoder[F, TweetUrl] =
    jsonEncoderOf
}

case class MediaUrl(url: String, media_type: String)
object MediaUrl {
  implicit val mediaUrlDecoder: Decoder[MediaUrl] = Decoder.forProduct2("url", "type")(MediaUrl.apply)
  implicit def mediaUrlEntityDecoder[F[_]: Sync]: EntityDecoder[F, MediaUrl] =
    jsonOf
  implicit val mediaUrlEncoder: Encoder[MediaUrl] = Encoder.forProduct2("url", "media_type")(m =>
    (m.url, m.media_type)
  )
  implicit def mediaUrlEntityEncoder[F[_]: Applicative]: EntityEncoder[F, MediaUrl] =
    jsonEncoderOf
}
