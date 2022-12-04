package competitive.util.json
import io.circe.*
import io.circe.parser.*
import scala.deriving.*

object Json:
    def parse[T](json: String)(implicit decoder: Decoder[T]): List[T] =
        val parsed = decode[List[T]](json)
        val value = parsed match
            case Right(value) => value
            case Left(error) => throw error
        value