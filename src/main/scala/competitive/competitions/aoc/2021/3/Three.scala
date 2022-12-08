package competitive.competitions.aoc.y2021
import competitive.competitions.aoc.AocProblem

object Three extends AocProblem(2021, 3):
    def parse(input: Vector[String]) = 
        input

    def binToDec(bin: String) = Integer.parseInt(bin, 2)
    override def part1(input: Vector[String]) =
        val parsed = parse(input)
        val mostCommon = (0 until parsed(0).length)
            .map(i => parsed.map(_(i)).groupBy(identity).maxBy(_._2.length)._1)
            .mkString
        val leastCommon = (0 until parsed(0).length)
            .map(i => parsed.map(_(i)).groupBy(identity).minBy(_._2.length)._1)
            .mkString
        (binToDec(mostCommon) * binToDec(leastCommon)).toString

    override def part2(input: Vector[String]) =
        val parsed = parse(input)
        val _mostCommon = (0 until parsed(0).length).map(i => parsed.map(_(i)).groupBy(identity))
        var mostCommon = _mostCommon.map(m =>
            println(s"0: ${m('0').length}, 1: ${m('1').length}")
            if m('0').length == m('1').length then '1' else m.maxBy(_._2.length)._1
        ).mkString
        val _leastCommon = (0 until parsed(0).length)
            .map(i => parsed.map(_(i)).groupBy(identity))
        var leastCommon = _leastCommon.map(m =>
            if m('0').length == m('1').length then '0' else m.minBy(_._2.length)._1
        ).mkString
        var mostCommonCandidates = parsed
        var leastCommonCandidates = parsed
        var currentIndex = 0
        println(s"mostCommon: $mostCommon, leastCommon: $leastCommon")
        while mostCommonCandidates.size > 1 do
            val _mostCommon = (0 until mostCommonCandidates(0).length).map(i => mostCommonCandidates.map(_(i)).groupBy(identity))
            val _leastCommon = (0 until mostCommonCandidates(0).length).map(i => mostCommonCandidates.map(_(i)).groupBy(identity))
            mostCommon = _mostCommon.map(m =>
                if m.isDefinedAt('0') && m.isDefinedAt('1') && m('0').length == m('1').length then 
                    '1' 
                else m.maxBy(_._2.length)._1
            ).mkString
            leastCommon = _leastCommon.map(m =>
                if m.isDefinedAt('0') && m.isDefinedAt('1') && m('0').length == m('1').length then 
                    '0' 
                else m.minBy(_._2.length)._1
            ).mkString
            mostCommonCandidates =  mostCommonCandidates.filter(_(currentIndex) == mostCommon(currentIndex))
            leastCommonCandidates =  leastCommonCandidates.filter(_(currentIndex) == leastCommon(currentIndex))
            currentIndex += 1
        (binToDec(mostCommon) * binToDec(leastCommon)).toString

