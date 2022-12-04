package competitive.util

object Input:
    def getInput(prompt: String, cond: String => Boolean, expl: String = "") =
        var valid = false
        var input = ""
        while !valid do
            println(prompt)
            input = scala.io.StdIn.readLine()
            valid = cond(input)
            if !valid && expl != "" then
                println(expl)
        input
