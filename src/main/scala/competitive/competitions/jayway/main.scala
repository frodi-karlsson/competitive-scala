package competitive.competitions.jayway

import competitive.competitions.jayway.*
import competitive.util.*

@main
def run =
    val problem = Input.getInput(
        "Which problem do you want to run?",
        _.matches("\\d+"), 
        "Please enter a problem number"
    ).toInt

    println(
        problem match
            case 1 => y2021.One.run
            case _ => throw new Exception("Invalid problem number")
    )



