//> using lib "com.softwaremill.sttp.client3::core:3.8.3"
package competitive.competitions.jayway

import scala.io.Source
import sttp.client3.*
import java.sql.*
import java.util.Arrays;
import java.util.Base64;
import java.io.{FileReader, File};

case class Parser(url: String):
    def getInput(year: Int, day: Int, inputNr: Int) =
        val req = basicRequest.get(uri"$url")
        val backend = HttpURLConnectionBackend()
        val response = req.send(backend)
        response.body match
            case Right(body) => body
            case Left(error) => throw new Exception(error)

    def apply(year: Int, day: Int, inputNr: Int = -1) = getInput(year, day, inputNr).toString