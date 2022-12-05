package competitive.util.json
import io.circe.*
import competitive.util.json.{Json as JsonUtil}

trait JsonProblem[I](json: String):
    implicit val decoder: Decoder[I]
    protected def parse(s: String): List[I] = JsonUtil.parse[I](s)(using decoder)
    lazy val parsed = parse(json)
    def solve(input: List[I]): String
    