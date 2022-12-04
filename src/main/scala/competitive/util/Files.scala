//> using lib "com.softwaremill.sttp.client3::core:3.8.3"
package competitive.util

import scala.io.Source
import sttp.client3.*

object Files:
    def fromURL(url: String, headers: (String, String)*): String =
        var req = basicRequest.get(uri"$url")
        for (header <- headers) do
            req = req.header(header._1, header._2)
        val backend = HttpURLConnectionBackend()
        val response = req.send(backend)
        response.body match
            case Right(body) => body
            case Left(error) => throw new Exception(error)

    def fromFile(path: String): String =
        val source = Source.fromFile(path)
        val lines = source.getLines().mkString("\n")
        source.close()
        lines