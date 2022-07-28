import scala.io.Source
import scala.util.control.Breaks.{breakable, break}

def execute(memory: Array[Int]): Unit = {
  var cursor: Int = 0
  var opcode: Int = -1

  // Registers for readability
  var a: Int = 0
  var b: Int = 0
  var c: Int = 0

  while (true) {
    opcode = memory(cursor)

    opcode match
      case 99 => return
      case 1 => {
        a = memory(cursor + 1)
        b = memory(cursor + 2)
        c = memory(cursor + 3)

        memory(c) = memory(a) + memory(b)
      }
      case 2 => {
        a = memory(cursor + 1)
        b = memory(cursor + 2)
        c = memory(cursor + 3)

        memory(c) = memory(a) * memory(b)
      }

    cursor += 4
  }
}

@main def main(): Unit = {
  val file = Source.fromFile("day_02.in")
  val code: Array[Int] = file.getLines().next().split(",").map(_.toInt).toArray
  file.close

  var memory: Array[Int] = Array[Int]()

  // Part 1

  memory = code.clone()
  memory(1) = 12
  memory(2) = 2
  execute(memory)

  println(s"Value at address 0: ${memory(0)}")

  // Part 2

  breakable {
    for (noun <- 0 to 99; verb <- 0 to 99) {

      memory = code.clone()
      memory(1) = noun
      memory(2) = verb
      execute(memory)

      if (19690720 == memory(0)) {
        println(s"Output: ${100 * noun + verb}")
        break
      }
    }
  }

}
