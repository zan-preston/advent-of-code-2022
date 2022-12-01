//> using scala "3.2.1"
//> using lib "org.typelevel::cats-effect:3.4.0"
//> using lib "co.fs2::fs2-core:3.4.0"
//> using lib "co.fs2::fs2-io:3.4.0"

import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import fs2.{Stream, text}
import fs2.io.file.{Files, Path}


object ElfCalories extends IOApp.Simple {

  val fileName: String = "day1-in.txt"
  val elves: IO[Unit] = 
    Files[IO].readUtf8Lines(Path(fileName))
    .map(line => line.trim.toIntOption)
    .foreach(Console[IO].println(_))
    .compile.drain

  def run: IO[Unit] = elves
}




