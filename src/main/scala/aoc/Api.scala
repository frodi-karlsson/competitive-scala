//> using lib "com.softwaremill.sttp.client3::core:3.8.3"
//> using lib "org.xerial:sqlite-jdbc:3.39.2.1"
//> using lib "net.java.dev.jna:platform:3.5.2"
//> using lib "org.typelevel::jawn-ast:1.3.2"
package aoc

import scala.io.Source
import sttp.client3.*
import java.sql.*
import java.util.Arrays;
import java.util.Base64;
import java.io.{FileReader, File};
import com.sun.jna.platform.win32.Crypt32Util;
import org.typelevel.jawn.ast.*
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import javax.crypto.spec.IvParameterSpec

object Api:
    def url(year: Int, day: Int) = s"https://adventofcode.com/$year/day/$day/input"
    def getInput(year: Int, day: Int, inputNr: Int) =
        val url = Api.url(year, day) + (if inputNr > -1 then s"/$inputNr" else "")
        val token = scala.io.Source.fromFile("./token").mkString
        val req = basicRequest.get(uri"$url").header("Cookie", s"session=$token}")
        val backend = HttpURLConnectionBackend()
        val response = req.send(backend)
        response.body match
            case Right(body) => body
            case Left(error) => throw new Exception(error)

    def apply(year: Int, day: Int, inputNr: Int = -1) = getInput(year, day, inputNr).split("\n").map(_.replaceAll("\r", "")).toVector
