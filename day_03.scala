import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.math.{min, max}

case class Position(var x: Int = 0, var y: Int = 0)

// Read and parse file
def parsePath(path: String): Array[Position] = {

  val parsed = ArrayBuffer[Position]()

  var dir: Char = '-'
  var range: Int = 0
  var currentPos = Position()
  parsed += currentPos.copy()

  for (step <- path.split(",")) {
    dir = step(0)
    range = step.substring(1).toInt

    dir match
      case 'R' => currentPos.x += range
      case 'L' => currentPos.x -= range
      case 'U' => currentPos.y += range
      case 'D' => currentPos.y -= range

    parsed += currentPos.copy()
  }

  // Ignore starting position (0, 0)
  dir = path(0)
  dir match
    case 'R' => parsed(0).x += 1
    case 'L' => parsed(0).x -= 1
    case 'U' => parsed(0).y += 1
    case 'D' => parsed(0).y -= 1

  parsed.toArray
}

// Partial solution for part 1
def getClosestIntersection(
    a1: Position,
    a2: Position,
    b1: Position,
    b2: Position
): Int = {

  var minDist: Int = -1

  val ax_min = min(a1.x, a2.x)
  val ax_max = max(a1.x, a2.x)

  val bx_min = min(b1.x, b2.x)
  val bx_max = max(b1.x, b2.x)

  val ay_min = min(a1.y, a2.y)
  val ay_max = max(a1.y, a2.y)

  val by_min = min(b1.y, b2.y)
  val by_max = max(b1.y, b2.y)

  val x_overlaps = !(ax_min > bx_max || bx_min > ax_max)
  val y_overlaps = !(ay_min > by_max || by_min > ay_max)

  if (x_overlaps && y_overlaps) {
    // Since there can be multiple overlapping positions of wires a and b,
    // we need to check all of them and return one closest to position (0, 0)
    val ox_min = max(ax_min, bx_min)
    val ox_max = min(ax_max, bx_max)

    val oy_min = max(ay_min, by_min)
    val oy_max = min(ay_max, by_max)

    minDist = 0
    minDist += (if (ox_max.abs < ox_min.abs) ox_max.abs else ox_min.abs)
    minDist += (if (oy_max.abs < oy_min.abs) oy_max.abs else oy_min.abs)
  }

  minDist
}

// Partial solution for part 2
def getStepsToIntersection(
    a1: Position,
    a2: Position,
    b1: Position,
    b2: Position
): Int = {

  var steps: Int = -1

  val ax_min = min(a1.x, a2.x)
  val ax_max = max(a1.x, a2.x)

  val bx_min = min(b1.x, b2.x)
  val bx_max = max(b1.x, b2.x)

  val ay_min = min(a1.y, a2.y)
  val ay_max = max(a1.y, a2.y)

  val by_min = min(b1.y, b2.y)
  val by_max = max(b1.y, b2.y)

  val x_overlaps = !(ax_min > bx_max || bx_min > ax_max)
  val y_overlaps = !(ay_min > by_max || by_min > ay_max)

  if (x_overlaps && y_overlaps) {
    // If there is more than one position overlapping
    // sum of steps will be the same for all overlapping
    // positions, so there is no need to check all the points

    val ox_min = max(ax_min, bx_min)
    val oy_min = max(ay_min, by_min)

    steps =
      (ox_min - a1.x).abs + (oy_min - a1.y).abs + (ox_min - b1.x).abs + (oy_min - b1.y).abs

  }

  steps
}

@main def main(): Unit = {
  val file = Source.fromFile("day_03.in")
  val lines = file.getLines()

  val parsedPathA = parsePath(lines.next())
  val parsedPathB = parsePath(lines.next())

  // Part 1

  var minDist: Int = -1

  for (
    i <- 0 until parsedPathA.length - 1;
    j <- 0 until parsedPathB.length - 1
  ) {

    val dist = getClosestIntersection(
      parsedPathA(i),
      parsedPathA(i + 1),
      parsedPathB(j),
      parsedPathB(j + 1)
    )

    if (dist != -1)
      if (minDist == -1) minDist = dist
      else minDist = min(minDist, dist)
  }

  println(s"Distance: ${minDist}")

  // Part 2

  var minSteps: Int = -1

  // Since first step is ommited - start counting from 1
  var stepsA: Int = 1
  var stepsB: Int = 1

  // Check all lines from wire a against all lines from wire b
  for (i <- 0 until parsedPathA.length - 1) {
    val a1 = parsedPathA(i)
    val a2 = parsedPathA(i + 1)

    for (j <- 0 until parsedPathB.length - 1) {
      val b1 = parsedPathB(j)
      val b2 = parsedPathB(j + 1)

      // Check if there is an intersection and if there is - count steps to it
      val addedSteps = getStepsToIntersection(a1, a2, b1, b2)

      // If there was an intersection, check if it's the intersection
      // with minimal steps counting from start
      if (addedSteps != -1)
        if (minSteps == -1) minSteps = stepsA + stepsB + addedSteps
        else minSteps = min(minSteps, stepsA + stepsB + addedSteps)

      // Move to another line of wire b
      stepsB += (b1.x - b2.x).abs + (b1.y - b2.y).abs
    }

    // Move to another line of wire a and reset b
    stepsA += (a1.x - a2.x).abs + (a1.y - a2.y).abs
    stepsB = 1
  }

  println(s"Steps: ${minSteps}")
}
