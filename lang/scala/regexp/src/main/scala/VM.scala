import scala.collection.mutable

trait VM {
  def run(code: List[Instruction], input: String): (Boolean, Int)
}

object RecVM extends VM {
  def run(code: List[Instruction], input: String): (Boolean, Int) = {
    var count = 0
    def step(pc: Int, sp: Int): Boolean = {
      count += 1

      if (pc >= code.length) return false
      code(pc) match {
        case Chara(c) => sp < input.length && input(sp) == c && step(pc + 1, sp + 1)
        case Match()  => sp == input.length
        case Jump(target) => step(target, sp)
        case Split(target1, target2) => step(target1, sp) || step(target2, sp)
      }
    }
    (step(0, 0), count)
  }
}

object QueueVM extends VM {
  private case class State(pc: Int, sp: Int)

  def run(code: List[Instruction], input: String): (Boolean, Int) = {
    val threads = mutable.Queue[State](State(0, 0))
    var count = 0

    while (threads.nonEmpty) {
      count += 1
      val thread = threads.dequeue
      code(thread.pc) match {
        case Chara(c) => if thread.sp < input.length && input(thread.sp) == c then
          threads.enqueue(State(thread.pc + 1, thread.sp + 1)) else ()
        case Match() => if thread.sp == input.length then return (true, count) else ()
        case Jump(target) =>
          threads.enqueue(State(target, thread.sp))
        case Split(target1, target2) =>
          threads.enqueue(State(target1, thread.sp))
          threads.enqueue(State(target2, thread.sp))
      }
    }
    (false, count)
  }
}

object KTVM extends VM {
  private case class State(pc: Int, sp: Int)

  def run(code: List[Instruction], input: String): (Boolean, Int) = {
    var finished = false
    var count = 0
    val threads = mutable.Map[Int, Set[Int]]() // sp -> set of pc

    for (i <- 0 to input.length) {
      threads(i) = Set()
    }
    threads(0) = threads(0) + 0

    for (i <- 0 to input.length) {
      while (!finished && threads(i).nonEmpty) {
        val pc = threads(i).head
        threads(i) = threads(i) - pc

        count += 1

        code(pc) match {
          case Chara(c) =>
            if i < input.length && input(i) == c then
              threads(i + 1) = threads(i + 1) + (pc + 1)
          case Match() =>
            if i == input.length then finished = true
          case Jump(target) =>
            threads(i) = threads(i) + target
          case Split(target1, target2) =>
            threads(i) = threads(i) + target1 + target2
        }
      }
    }

    (finished, count)
  }
}