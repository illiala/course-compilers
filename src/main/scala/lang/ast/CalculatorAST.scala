package lang.ast

import scala.io.StdIn

//TODO: FizzBuzz
object CalculatorAST {
  def main(args: Array[String]): Unit = {
    // (13 * 6 + prompt("Input num: ") * 8)
    val expression = Plus(Times(Literal(13), Literal(6)), Times(GetNumber(), Literal(4)))
    val expressionWithPrint = Print(expression)
    val badExpression = Plus(Times(Literal(13), Literal(6)), Times(Print(Literal(7)), Literal(4)))

    println(expressionWithPrint)
    // TODO: assumes inputting 3 to GetNumber()
    assert(expression.eval().contains(90))

    println
    println(Node.optimize(expressionWithPrint))
    assert(Node.optimize(expressionWithPrint) == Print(Plus(Literal(78),Times(GetNumber(),Literal(4)))))

    println
    println(expressionWithPrint.compile())
    assert(expressionWithPrint.compile() == """console.log((13 * 6 + prompt("Input num: ") * 4))""")

    println
    println(expressionWithPrint.typeCheck())
    assert(expressionWithPrint.typeCheck())
    assert(!badExpression.typeCheck())

    println
    println(expressionWithPrint.fixEvens())
    println(expressionWithPrint.fixEvens().compile())
    assert(expressionWithPrint.fixEvens() ==
      Print(Plus(
        Times(Literal(13),Plus(Literal(3),Literal(3))),
        Times(
          GetNumber(),
          Plus(Plus(Literal(1),Literal(1)),Plus(Literal(1),Literal(1)))))))
  }
}

sealed trait Node {
  def eval(): Option[Int]

  // to JavaScript...
  def compile(): String

  def typeCheck(): Boolean
  def fixEvens(): Node
}

object Node {
  def optimize(root: Node): Node = {
    root match {
      case Print(value) => Print(optimize(value))
      case Plus(left, right) => (left, right) match {
        case (Literal(a), Literal(b)) => Literal(a + b)
        case _ => Plus(optimize(left), optimize(right))
      }
      case Times(left, right) => (left, right) match {
        case (Literal(a), Literal(b)) => Literal(a * b)
        case _ => Times(optimize(left), optimize(right))
      }
      case GetNumber() | Literal(_) => root
    }
  }
}

case class Print(value: Node) extends Node {
  override def eval(): Option[Int] = {
    print(value.eval())
    None
  }

  override def compile(): String = s"console.log(${value.compile()})"

  override def typeCheck(): Boolean = value.typeCheck()

  override def fixEvens(): Node = Print(value.fixEvens())
}

case class Plus(left: Node, right: Node) extends Node {
  override def eval(): Option[Int] = {
    for {
      leftRes <- left.eval()
      rightRes <- right.eval()
    } yield leftRes + rightRes
  }

  override def compile(): String = s"(${left.compile()} + ${right.compile()})"

  override def typeCheck(): Boolean = {
    (left, right) match {
      case (Print(_), _) | (_, Print(_)) => {
        println(s"Type error in ${this.toString}")
        false
      }
      case _ => left.typeCheck() && right.typeCheck()
    }
  }

  override def fixEvens(): Node = Plus(left.fixEvens(), right.fixEvens())
}

case class Times(left: Node, right: Node) extends Node {
  override def eval(): Option[Int] = {
    for {
      leftRes <- left.eval()
      rightRes <- right.eval()
    } yield leftRes * rightRes
  }

  override def compile(): String = s"${left.compile()} * ${right.compile()}"

  override def fixEvens(): Node = Times(left.fixEvens(), right.fixEvens())

  override def typeCheck(): Boolean = {
    (left, right) match {
      case (Print(_), _) | (_, Print(_)) => {
        println(s"Type error in ${this.toString}")
        false
      }
      case _ => left.typeCheck() && right.typeCheck()
    }
  }
}

case class GetNumber() extends Node {
  override def eval(): Option[Int] = {
    print("Input num: ")
    Some(StdIn.readLine().toInt)
  }

  override def compile(): String = s"""prompt("Input num: ")"""

  override def typeCheck(): Boolean = true

  override def fixEvens(): Node = this
}

case class Literal(value: Int) extends Node {
  override def eval(): Option[Int] = Some(value)

  override def compile(): String = value.toString

  override def typeCheck(): Boolean = true

  override def fixEvens(): Node = {
    if (value % 2 == 0) {
      Plus(Literal(value / 2).fixEvens(), Literal(value / 2).fixEvens())
    } else {
      Literal(value)
    }
  }
}
