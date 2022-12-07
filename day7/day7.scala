//> using scala "3.2.1"
//> using lib "org.typelevel::cats-effect:3.4.0"
//> using lib "co.fs2::fs2-core:3.4.0"
//> using lib "co.fs2::fs2-io:3.4.0"

import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import fs2.{Stream, text}
import fs2.io.file.{Files, Path}


object ConsoleStream extends IOApp.Simple {

  sealed trait Line
  final case class CD(dir: String) extends Line
  case object LS extends Line
  final case class File(size: Int, name: String) extends Line
  final case class Directory(name: String) extends Line

  final case class DirSize(name: String, size: Int)
  final case class State(sizes: List[DirSize])

  def parse(line: String) : Line = {
    line.split("""\s""") match {
      case Array("$", "cd", name) => CD(name)
      case Array("$", "ls") => LS
      case Array("dir", name) => Directory(name)
      case Array(size, name) => File(size.toInt, name)
    }
  }



  // val fileName: String = "day7-in.txt"
  val fileName: String = "day7-in-sample.txt"
  val console: IO[Unit] = 
    Files[IO].readUtf8Lines(Path(fileName))
    .filter(_.nonEmpty)
    .map(parse)
    .scan(State(List(DirSize("", 0))))((state, line: Line) => line match {
      case CD("/") => State(List(DirSize("", 0)))
      case CD("..") => state.sizes match {
        case current :: parent :: rest => State(parent.copy(size = parent.size + current.size) +: rest)
      }
      case CD(name) => State(DirSize(name, 0) +: state.sizes)
      case File(size, _) => state.sizes match {
        case current :: rest => State(DirSize(current.name, current.size + size) +: rest)
      }
      case _ => state
    } )
    .foreach(Console[IO].println(_))
    .compile.drain

  def run: IO[Unit] = console
}

