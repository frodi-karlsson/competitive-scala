package competitive.competitions.aoc
import competitive.util.*

@main
def run =
    // change this to reflect year
    import  competitive.competitions.aoc.y2022.*
    val days = 
        Vector(
            One, Two, Three, Four, Five, 
            Six, Seven, Eight, Nine, Ten, 
            Eleven, Twelve, Thirteen, Fourteen, Fifteen, 
            Sixteen, Seventeen, Eighteen, Nineteen, Twenty, 
            TwentyOne, TwentyTwo, TwentyThree, TwentyFour, TwentyFive
        )
    val input = Input.getInput(
        "Which day do you want to run?",
        s => s.matches("\\d+") && s.toInt > 0 && s.toInt <= days.length,
        "Invalid input, please enter a number between 1 and 25"
        )
    val day = input.toInt
    days(day - 1).solve

//@main // uncomment this to generate new year/days
def generateAoc =
    val year = 
        Input.getInput(
            "Which year do you want to generate?",
            s => s.matches("\\d+") && s.toInt >= 2000 && s.toInt <= 2100,
            "Invalid input, please enter a number between 2000 and 2100"
        ).toInt
    val input = 
        Input.getInput(
            "Which days do you want to generate?",
            s => s.forall(i => i.isDigit || i.isSpaceChar) || s.toLowerCase == "all", 
            "Invalid input, please enter a number between 1 and 25")
    val days = 
        input match
            case "all" => (1 to 25).toVector
            case _ => input.split(" ").map(_.toInt).toVector

    def dayToString(day: Int) =
        day match
            case 1 => "One"
            case 2 => "Two"
            case 3 => "Three"
            case 4 => "Four"
            case 5 => "Five"
            case 6 => "Six"
            case 7 => "Seven"
            case 8 => "Eight"
            case 9 => "Nine"
            case 10 => "Ten"
            case 11 => "Eleven"
            case 12 => "Twelve"
            case 13 => "Thirteen"
            case 14 => "Fourteen"
            case 15 => "Fifteen"
            case 16 => "Sixteen"
            case 17 => "Seventeen"
            case 18 => "Eighteen"
            case 19 => "Nineteen"
            case 20 => "Twenty"
            case 21 => "TwentyOne"
            case 22 => "TwentyTwo"
            case 23 => "TwentyThree"
            case 24 => "TwentyFour"
            case 25 => "TwentyFive"
            case _ => throw new Exception("Invalid day")

    def getCode(day: Int) = 
        val dayStr = dayToString(day)
        s"""package competitive.competitions.aoc.y$year
import competitive.competitions.aoc.AocProblem

object $dayStr extends AocProblem($year, $day):
    def parse(input: Vector[String]) = 
        ???

    override def part1(input: Vector[String]) =
        ???

    override def part2(input: Vector[String]) =
        ???

"""
    def generateDay(day: Int) =
        val basePath = s"./src/main/scala/competitive/competitions/aoc/$year"
        val path = s"$basePath/$day/"
        val testFile = new java.io.File(s"$path/test")
        val sol1File = new java.io.File(s"$path/sol1")
        val sol2File = new java.io.File(s"$path/sol2")
        val dayFile = new java.io.File(s"$path/${dayToString(day)}.scala")
        val code = getCode(day)
        // create the directory if it doesn't exist
        val dir = new java.io.File(path)
        dir.mkdirs()
        //create the files if they don't exist
        for file <- Vector(testFile, sol1File, sol2File, dayFile) do
            if !file.exists then
                file.createNewFile

        //write code to file
        val writer = new java.io.FileWriter(dayFile)
        writer.write(code)
        writer.close()

