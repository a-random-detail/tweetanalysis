package com.example.tweetanalysis

import cats.effect.Sync
import cats.Applicative
import cats.implicits._
import io.circe._
import io.circe.generic.semiauto._
import org.http4s.circe._
import org.http4s.{EntityDecoder, EntityEncoder}
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._




case class Tweet(created_at: String, text: String, entities: Entities)
object Tweet {
  implicit val tweetDecoder: Decoder[Tweet] = deriveDecoder[Tweet]
  implicit def tweetEntityDecoder[F[_]: Sync]: EntityDecoder[F, Tweet] =
    jsonOf
  implicit val tweetEncoder: Encoder[Tweet] = deriveEncoder[Tweet]
  implicit def tweetEntityEncoder[F[_]: Applicative]: EntityEncoder[F, Tweet] =
    jsonEncoderOf
}

case class Entities(hashtags: Option[List[Hashtag]] = Some(List()), urls: Option[List[TweetUrl]] = Some(List()), media: Option[List[MediaUrl]] = Some(List()))
object Entities {
  implicit val customConfig: Configuration = Configuration.default.withDefaults
  implicit val entitiesDecoder: Decoder[Entities] = deriveDecoder[Entities]
  implicit def entitiesEntityDecoder[F[_]: Sync]: EntityDecoder[F, Entities] =
    jsonOf
  implicit val entitiesEncoder: Encoder[Entities] = deriveEncoder[Entities]
  implicit def entitiesEntityEncoder[F[_]: Applicative]: EntityEncoder[F, Hashtag] =
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

case class TweetUrl(url: String, expanded_url: String)
object TweetUrl {
  implicit val tweetUrlDecoder: Decoder[TweetUrl] = deriveDecoder[TweetUrl]
  implicit def tweetUrlEntityDecoder[F[_]: Sync]: EntityDecoder[F, TweetUrl] =
    jsonOf
  implicit val tweetUrlEncoder: Encoder[TweetUrl] = deriveEncoder[TweetUrl]
  implicit def tweetUrlEntityEncoder[F[_]: Applicative]: EntityEncoder[F, TweetUrl] =
    jsonEncoderOf
}

case class MediaUrl(url: String, media_type: String, indices: List[Int])
object MediaUrl {
  implicit val mediaUrlDecoder: Decoder[MediaUrl] = Decoder.forProduct3("url", "type", "indices")(MediaUrl.apply)
  implicit def mediaUrlEntityDecoder[F[_]: Sync]: EntityDecoder[F, MediaUrl] =
    jsonOf
  implicit val mediaUrlEncoder: Encoder[MediaUrl] = Encoder.forProduct3("url", "type", "indices")(m =>
    (m.url, m.media_type, m.indices)
  )
  implicit def mediaUrlEntityEncoder[F[_]: Applicative]: EntityEncoder[F, MediaUrl] =
    jsonEncoderOf
}
