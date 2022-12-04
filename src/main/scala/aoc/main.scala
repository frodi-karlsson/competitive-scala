package aoc
@main
def run =
    // change this to reflect year
    import aoc.y2022.*
    val days = 
        Vector(
            One, Two, Three, Four, Five, 
            Six, Seven, Eight, Nine, Ten, 
            Eleven, Twelve, Thirteen, Fourteen, Fifteen, 
            Sixteen, Seventeen, Eighteen, Nineteen, Twenty, 
            TwentyOne, TwentyTwo, TwentyThree, TwentyFour, TwentyFive
        )
    var inputValid = false
    while !inputValid do
        println("Which day do you want to run?")
        val input = scala.io.StdIn.readLine()
        inputValid = input.forall(_.isDigit)
        if inputValid then
            val day = input.toInt
            if day > 0 && day <= days.length then
                days(day - 1).solve
            else
                println("Invalid day")
                inputValid = false
        else
            println("Invalid input")



//@main // uncomment this to generate new year/days
def generate =
    //get year
    var inputValid = false
    var year = 0
    while !inputValid do
        println("Which year do you want to generate?")
        val input = scala.io.StdIn.readLine()
        inputValid = input.forall(_.isDigit)
        if inputValid then
            year = input.toInt
            if year < 2000 || year > 2100 then
                println("Invalid year")
                inputValid = false
        else
            println("Invalid input")
    
    inputValid = false
    while !inputValid do
        println("Which days do you want to generate?")
        val input = scala.io.StdIn.readLine()
        inputValid = input.forall(i => i.isDigit || i.isSpaceChar) || input == "all"
        if inputValid then
            val days = 
                if input != "all" then 
                    input.split(" ").map(_.toInt).toVector 
                else 
                    (1 to 25).toVector
            days.foreach(generateDay)
        else
            println("Invalid input")

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
        s"""package aoc.y$year
import aoc.Problem

object $dayStr extends Problem($year, $day):
    def parse(input: Vector[String]) = 
        ???

    override def part1(input: Vector[String]) =
        ???

    override def part2(input: Vector[String]) =
        ???

"""
    def generateDay(day: Int) =
        val basePath = s"./src/main/scala/aoc/$year"
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

