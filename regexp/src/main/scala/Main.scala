object Main {
  def main(args: Array[String]): Unit = {
    val re = Concat( // a(b|c)*
      Literal('a'),
      Star(
        Alternation(
          Literal('b'),
          Literal('c')
        )
      )
    )

    val code = re.compile
    println(RecVM.run(code, "abbbb"))
    println(RecVM.run(code, "abc"))
    println(RecVM.run(code, "a"))
    println(RecVM.run(code, "absss"))
    println(RecVM.run(code, "accaa"))
    println(RecVM.run(code, "abbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))

    println("-----")
    
    println(QueueVM.run(code, "abbbb"))
    println(QueueVM.run(code, "abc"))
    println(QueueVM.run(code, "a"))
    println(QueueVM.run(code, "absss"))
    println(QueueVM.run(code, "accaa"))
    println(RecVM.run(code, "abbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))

    println("-----")

    println(KTVM.run(code, "abbbb"))
    println(KTVM.run(code, "abc"))
    println(KTVM.run(code, "a"))
    println(KTVM.run(code, "absss"))
    println(KTVM.run(code, "accaa"))
    println(RecVM.run(code, "abbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))
  }
}
