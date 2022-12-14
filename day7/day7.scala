//> using scala "3.2.1"
//> using lib "org.typelevel::cats-effect:3.4.0"
//> using lib "co.fs2::fs2-core:3.4.0"
//> using lib "co.fs2::fs2-io:3.4.0"

import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import fs2.{Stream, Pipe, Pull, text}
import fs2.io.file.{Files, Path}


object ConsoleStream extends IOApp.Simple {

  sealed trait Line
  final case class CD(dir: String) extends Line
  case object LS extends Line
  final case class File(size: Int, name: String) extends Line
  final case class Directory(name: String) extends Line

  final case class DirSize(name: String, size: Int)
  final case class State(sizes: List[DirSize])

  def parse(in: String) : Line = {
    in.split("""\s""") match {
      case Array("$", "cd", name) => CD(name)
      case Array("$", "ls") => LS
      case Array("dir", name) => Directory(name)
      case Array(size, name) => File(size.toInt, name)
    }
  }

  val dirInterpreter: Pipe[IO, Line, DirSize] = { (in : Stream[IO, Line]) => 

    def pull(in: Stream[IO, Line], state: State): Pull[IO, DirSize, Unit] = {
      in.pull.uncons1.flatMap{
        case Some((line, rest)) => line match {
          case CD("/") => pull(rest, State(List(DirSize("/", 0))))
          case CD("..") => state.sizes match {
            case current :: parent :: tail => Pull.output1(current).flatMap(_ => pull(rest, State(parent.copy(size = parent.size + current.size) +: tail)))
          }
          case CD(name) => pull(rest, State(DirSize(name, 0) +: state.sizes))
          case File(size, _) => state.sizes match {
            case current :: tail => pull(rest, State(DirSize(current.name, current.size + size) +: tail))
          }
          case _ => pull(rest, state)
        }
        case None => state.sizes match {
          case current :: parent :: tail => Pull.output1(current).flatMap(_ => pull(Stream.empty, State(parent.copy(size = parent.size + current.size) +: tail)))
          case current :: Nil => Pull.output1(current).flatMap(_=> Pull.done)
          case Nil => Pull.done
        }
      }
    }

    pull(in, State(List(DirSize("", 0)))).stream

  }


  // val fileName: String = "day7-in.txt"
  val fileName: String = "day7-in.txt"
  val result: IO[Int] = 
    Files[IO].readUtf8Lines(Path(fileName))
    .filter(_.nonEmpty)
    .map(parse)
    .through(dirInterpreter)
    .filter(_.size <= 100000)
    .foldMap(_.size) // same as .fold(0){(acc, in) => acc + in.size}
    // .foreach(Console[IO].println(_))
    .compile.lastOrError

  def run: IO[Unit] = result.flatMap(Console[IO].println(_))
}

