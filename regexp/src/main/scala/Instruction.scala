sealed trait Instruction

case class Chara(c: Char) extends Instruction
case class Match() extends Instruction
case class Jump(target: Int) extends Instruction
case class Split(target1: Int, target2: Int) extends Instruction
