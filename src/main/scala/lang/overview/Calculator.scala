package lang.overview

import scala.collection.mutable
import scala.util.matching.Regex

// TODO: unary operator
object Calculator {

  def main(args: Array[String]): Unit = {
    val expressions = List(
      //"1 - (2 * 3 + 4) * (2 + 6 / 2 - 4)",
      """(-
        |   1
        |   (*
        |     (+ (* 2 3) 4)
        |     (- (+ 2 (/ 6 2)) 4)
        |   )
        |)""".stripMargin,
      //"54 * (22.4 / 2) - 123 + 2"
      """(+
        |   (-
        |     (*
        |       54
        |       (/ 22.4 2)
        |     )
        |     123
        |   )
        |   2
        |)""".stripMargin
    )
    val expectedResults = List(-9.0, 483.8.toFloat)

    (expressions zip expectedResults).foreach { case (exp, expectedResult) =>
      println(Scanner.scan(exp))

      val rootAST: Node = new Parser(Scanner.scan(exp)).parse()
      println(rootAST)
      val evalResult = rootAST.eval()
      println(s"Eval result: $evalResult")
      assert(evalResult == expectedResult)

      val instructions = byteCode(rootAST)
      println(instructions)
      val vmResult = virtualMachine(instructions)
      println(s"VM result: $vmResult")
      assert(evalResult == vmResult)
    }
  }

  // ~~~~ Scanning ~~~~

  object TokenType extends Enumeration {
    type TokenType = Value
    val LEFT_PAREN, RIGHT_PAREN, DOT, MINUS, PLUS, SLASH, STAR, NUMBER = Value
  }
  import Calculator.TokenType._
  case class Token(tokenType: TokenType, value: String)

  object Scanner {
    val mapping: Map[String, TokenType] = Map(
      "(" -> LEFT_PAREN,
      ")" -> RIGHT_PAREN,
      "." -> DOT,
      "-" -> MINUS,
      "+" -> PLUS,
      "/" -> SLASH,
      "*" -> STAR
    )

    def scan(source: String): List[Token] = {
      val pattern: Regex = "([0-9]+\\.[0-9]+)|([0-9]+)|([-\\+\\*\\/\\(\\)\\.])".r

      pattern.findAllIn(source).map { matched =>
        if (mapping.contains(matched)) {
          Token(mapping(matched), matched)
        } else {
          Token(NUMBER, matched)
        }
      }.toList
    }
  }

  trait Node {
    def eval(): Float
  }
  case class Number(value: Float) extends Node {
    override def eval(): Float = value
  }
  case class BinaryExpr(left: Node, right: Node, op: TokenType) extends Node {
    override def eval(): Float = {
      op match {
        case TokenType.PLUS => left.eval() + right.eval()
        case TokenType.MINUS => left.eval() - right.eval()
        case TokenType.STAR => left.eval() * right.eval()
        case TokenType.SLASH => left.eval() / right.eval()
      }
    }
  }

  // ~~~ Parsing ~~~

  class Parser(tokens: List[Token]) {
    //TODO: try to make it functional / immutable
    var current = 0

    //val LEFT_PAREN, RIGHT_PAREN, MINUS, PLUS, SLASH, STAR, NUMBER = Value
    def parse(): Node = {
      expression()
    }

    def expression(): Node = {
      if (check(TokenType.LEFT_PAREN)) {
        // expression in parens
        moveForward()
        val op = moveForward()
        val left = expression()
        val right = expression()
        if (check(TokenType.RIGHT_PAREN)) {
          moveForward()
          BinaryExpr(left, right, op.tokenType)
        } else {
          // shouldn't happen
          throw new IllegalStateException("No right paren")
        }
      } else {
        // only number is possible
        val numToken = moveForward()
        Number(numToken.value.toFloat)
      }
    }

    private def check(tokenType: TokenType): Boolean =
      !isAtEnd() && currentToken().tokenType == tokenType

    private def moveForward(): Token = {
      if (!isAtEnd()) current += 1
      previousToken()
    }
    private def isAtEnd(): Boolean = current >= tokens.length
    private def currentToken() = tokens(current)
    private def previousToken() = tokens(current - 1)
  }


  // ~~~ Byte Code ~~~~

  val PUSH_COMMAND = "PUSH"
  def byteCode(root: Node): List[String] =
    root match {
      case Number(value) => List(PUSH_COMMAND, value.toString)
      case BinaryExpr(left, right, op) =>
        byteCode(left) ++ byteCode(right) ++ List(op.toString)
    }

  // ~~~~ Virtual Machine ~~~~~

  def virtualMachine(instructions: List[String]): Float = {
    val stack = mutable.Stack[Float]()
    var current = 0
    while (current < instructions.size) {
      instructions(current) match {
        case PUSH_COMMAND => {
          stack.push(instructions(current + 1).toFloat)
          current += 1
        }
        case "PLUS" => stack.push(stack.pop() + stack.pop())
        case "STAR" => stack.push(stack.pop() * stack.pop())
        case "MINUS" => {
          val second = stack.pop()
          val first = stack.pop()
          stack.push(first - second)
        }
        case "SLASH" => {
          val second = stack.pop()
          val first = stack.pop()
          stack.push(first / second)
        }
        case _ => println("Unknown command!")
      }
      current += 1
    }
    // and the answer is..
    stack.pop()
  }
}
