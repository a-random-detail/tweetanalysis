package com.example.tweetanalysis

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object TWStreamApp extends IOApp {
  def run(args: List[String]) =
    (new TWStream[IO]).run.as(ExitCode.Success)
}
