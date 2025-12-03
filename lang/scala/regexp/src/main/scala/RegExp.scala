sealed trait RegExp {
  // label: 次の命令のラベル(行数)
  def _compile(label: Int): (List[Instruction], Int)
  def compile: List[Instruction] = _compile(0)._1 :+ Match()
}

case class Empty() extends RegExp {
  def _compile(label: Int): (List[Instruction], Int) = (List(), label)
}

case class Literal(c: Char) extends RegExp {
  def _compile(label: Int): (List[Instruction], Int) = (List(Chara(c)), label + 1)
}

case class Concat(r1: RegExp, r2: RegExp) extends RegExp {
  def _compile(label: Int): (List[Instruction], Int) = {
    val (code1, nextLabel1) = r1._compile(label)
    val (code2, nextLabel2) = r2._compile(nextLabel1)
    (code1 ++ code2, nextLabel2)
  }
}

case class Alternation(r1: RegExp, r2: RegExp) extends RegExp {
  override def toString: String = s"(${r1.toString}|${r2.toString})"

  def _compile(label: Int): (List[Instruction], Int) = {
    val (code1, nextLabel1) = r1._compile(label + 1)
    val (code2, nextLabel2) = r2._compile(nextLabel1 + 1)
    val splitInst = Split(label + 1, nextLabel1 + 1)
    val jumpInst = Jump(nextLabel2)
    (splitInst :: code1 ++ (jumpInst :: code2), nextLabel2)
  }
}

case class Star(r: RegExp) extends RegExp {
  override def toString: String = s"(${r.toString})*"

  def _compile(label: Int): (List[Instruction], Int) = {
    val (code, nextLabel) = r._compile(label + 1)
    val splitInst = Split(label + 1, nextLabel + 1)
    val jumpInst = Jump(label)
    (splitInst :: code ++ (jumpInst :: Nil), nextLabel + 1)
  }
}