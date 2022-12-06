package competitive.competitions.aoc.y2022
import competitive.competitions.aoc.AocProblem
import collection.mutable.Stack

object Five extends AocProblem(2022, 5):
    def parse(input: Vector[String]) = 
        val stacksIndex = input.indexWhere(!_.contains("["))
        val inputWithoutStacks = input.take(stacksIndex) ++ input.drop(stacksIndex + 1)
        val boxLines = inputWithoutStacks.take(inputWithoutStacks.indexWhere(_.isEmpty))
        val boxes: Vector[Stack[Char]] = 
            (for line <- boxLines.map(_.zipWithIndex.filter(_._2 % 4 == 1).map(_._1)).transpose yield
                Stack.from(line.toVector.filterNot(_ == ' '))).toVector
        val instructions: Vector[(Int, Int, Int)] = 
            inputWithoutStacks
                .drop(stacksIndex + 1).map(_.split(" ").toVector).map(i => (i(1).toInt, i(3).toInt, i(5).toInt))
        (boxes, instructions)
    
    override def part1(input: Vector[String]) =
        val parsed = parse(input)
        val (stack, instructions) = parsed
        val execution = 
            (args: (Int, Int, Int)) =>
                for i <- 0 until args._1 do
                    stack(args._3 - 1).push(stack(args._2 - 1).pop())

        instructions.foreach(execution(_))
        stack.map(_.pop()).mkString("")

    override def part2(input: Vector[String]) =
        val parsed = parse(input)
        val (stack, instructions) = parsed
        val execution = 
            (args: (Int, Int, Int)) =>
                val boxes = (for i <- 0 until args._1 yield stack(args._2 - 1).pop()).toVector.reverse   
                for i <- 0 until args._1 do
                    stack(args._3 - 1).push(
                        boxes(i)
                    )

        instructions.foreach(execution(_))
        stack.map(_.pop()).mkString("")