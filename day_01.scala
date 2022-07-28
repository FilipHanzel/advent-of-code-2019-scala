import scala.io.Source

def calcCost(mass: Int): Int = mass / 3 - 2

@main def main(): Unit = {
  val file = Source.fromFile("day_01.in")
  val puzzleInput: List[Int] = file.getLines().map(_.toInt).toList
  file.close

  // Part 1

  var totalFuel: Int = 0
  for (inp <- puzzleInput)
    totalFuel += calcCost(inp)

  println(s"Total fuel: ${totalFuel}")

  // Part 2

  totalFuel = 0
  var mass: Int = 0

  for (inp <- puzzleInput) {

    mass = calcCost(inp)

    while (mass > 0) do {
      totalFuel += mass
      mass = calcCost(mass)
    }
  }

  println(s"Total fuel: ${totalFuel}")
}
