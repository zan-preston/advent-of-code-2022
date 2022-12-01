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
    // convert each line into an Option[Int]
    .map(line => line.trim.toIntOption)
    // Split stream into Chunks delimited by None (Each Chunk == 1 elf)
    .split(_ == None)
    // Sum the calories each elf is holding
    .map((x => x.foldLeft(0)((acc, opt) => acc + opt.get)))
    // Fold to find the highest number of calories
    .fold1(math.max)
    // Print the single element remaining in the stream
    .foreach(Console[IO].println(_))
    .compile.drain

  def run: IO[Unit] = elves
}




