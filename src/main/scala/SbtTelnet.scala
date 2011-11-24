package sbttelnet

import sbt._
import complete.{JLineCompletion, Parser}
import Keys._
import sbt.CommandSupport._
import java.net.{Socket, ServerSocket}
import java.lang.String
import java.io.{PrintWriter, BufferedReader, InputStreamReader}

object Telnet {

  def TelnetShell = "telnet"
  def TelnetShellBrief = TelnetShellDetailed
  def TelnetShellDetailed = "Provides a prompt via an open TCP port from which commands can be run."
  def telnetClient = AttributeKey[Socket]("telnet-client")

  val telnetPort = java.lang.Integer.getInteger("sbt.telnet.port", 1337)

  object TelnetReader {
    val Completion = "?"
    val Command = "!"
    val UTF8 = "UTF-8"

    val socket = new ServerSocket(telnetPort)
  }

  class TelnetReader(client: Socket, parser: Parser[() => State]) extends LineReader {
    import TelnetReader._
    import JLineCompletion._

    def readLine(prompt: String, mask: Option[Char]) = {
      lazy val completor = parserAsCompletor(parser)

      val in = new BufferedReader(new InputStreamReader(client.getInputStream, UTF8))
      val out = new PrintWriter(client.getOutputStream, true)

      var toRun: Option[String] = None

      while (toRun.isEmpty) {
        Option(in.readLine).map(l => (l.take(1), l.drop(1))) match {
          case Some((Completion, line)) =>
            val (insert, display) = completor(line, 0)
            val common = commonPrefix(insert)
            val response = "?" + (common +: display).mkString("@@")
            out.println(response)
          case Some((Command, line)) =>
            toRun = Some(line)
          case Some(_, _) => None
          case None => None
        }
      }
      toRun
    }
  }

  def telnetShell = Command.command(TelnetShell, TelnetShellBrief, TelnetShellDetailed) { case s: State =>
  		val history = (s get historyPath.key) getOrElse Some((s.baseDir / ".history").asFile)
      val client = (s get telnetClient) getOrElse TelnetReader.socket.accept()

  		val reader = new TelnetReader(client, s.combinedParser)
  		val line = reader.readLine("")
  		val ss = line match {
  			case Some(line) =>
  				val newState = s.copy(onFailure = Some(TelnetShell), remainingCommands = line +: TelnetShell +: s.remainingCommands)
  				if(line.trim.isEmpty) newState else newState.clearGlobalLog
  			case None => s
  		}
      ss put (telnetClient, client)
  	}

  lazy val settings = Seq[Setting[_]](
    commands += telnetShell
  )

}
