package pp201802.proj.MyParser
import pp201802.proj.Data.DataBundle._
import pp201802.proj.Lexer._

import collection.mutable.{ListBuffer, Stack}

class MyParserException(val msg: String) extends Exception

object MyParser {

  def getPRIndex(slice: List[Token]): Int = {
    val iter = slice.iterator
    val stack = new Stack[Token]
    var index: Int = 0

    while (iter.hasNext) {
      iter.next() match {
        case lp: TLPAREN =>
          stack.push(lp)
        case rp: TRPAREN =>
          if (stack.isEmpty)
            return index
          stack.pop
        case _ =>
      }

      index += 1
    }

    throw new MyParserException("Corresponding RP not found")
  }

  private[this] def readName(token: Token): String = {
    token match {
      case TVNAME(s: String) => s
      case t: Token => throw new MyParserException(s"Expected TVNAME, got ${t}")
    }
  }

  def parseArg(tokens: List[Token], offset: Int): (Arg, Int) = {
    tokens(0) match {
      case TVNAME(s: String) => (AVname(s), offset + 1)
      case TNNAME(s: String) => (ANname(s), offset + 1)
      case TLPAREN() =>
        val slice = tokens.slice(1, tokens.size)
        (parseArg(tokens.slice(1, getPRIndex(slice) + 1), offset + 2)) // todo: use 1 or 2?
    }
  }

  private[this] def parseBeta(tokens: List[Token], shift: Int = 0): (Bind, Int) = {
    tokens(0) match {
      case TLPAREN() =>
        val slice = tokens.slice(1, tokens.size)
        (parseBeta(tokens.slice(1, getPRIndex(slice) + 2), shift + 2))
      case TLAZYVAL() =>
        val name = readName(tokens(1))
        val (e, i) = parseExpression(tokens.slice(2, tokens.size), 2)
        (BLval(name, e), i + shift)
      case TVAL() =>
        val name = readName(tokens(1))
        val (e, i) = parseExpression(tokens.slice(2, tokens.size), shift + 2) // todo: should come 11
        (BVal(name, e), i)
      case TDEF() =>
        val TDEF_SHIFT = 3
        val name = readName(tokens(1))
        val slice = tokens.slice(TDEF_SHIFT, tokens.size) // Eat first LPAREN
        val endARP = getPRIndex(slice) + TDEF_SHIFT + 1

        val alphasSlice = tokens.slice(TDEF_SHIFT, endARP - 1)
        val alphas: List[Arg] = parseArgs(alphasSlice)

        val (e, i) = parseExpression(tokens.slice(endARP, tokens.size), endARP)
        (BDef(name, alphas, e), i + shift)
    }
  }

  private[this] def parseArgs(tokens: List[Token]): List[Arg] = {
    val buf = ListBuffer[Arg]()
    var offset: Int = 0

    while (offset < tokens.size) {
      val (a, i) = parseArg(tokens.slice(offset, tokens.size), offset)
      buf += a
      offset = i
    }

    buf.toList
  }

  private[this] def parseBettas(parenthesized: List[Token]): List[Bind] = {
    val buf = ListBuffer[Bind]()
    var offset: Int = 0
    val tokens = parenthesized.slice(1, parenthesized.length - 1)

    while (offset < tokens.size) {
      val (b, i) = parseBeta(tokens.slice(offset, tokens.size), offset)
      buf += b
      offset = i
    }

    buf.toList
  }

  def parseExpressions(tokens: List[Token]): List[Expr] = {
    val buf = ListBuffer[Expr]()
    var offset: Int = 0

    while (offset < tokens.size) {
      val (e, i) = parseExpression(tokens.slice(offset, tokens.size), offset)
      buf += e
      offset = i
    }

    buf.toList
  }

  private[this] def parseExpression(tokens: List[Token], shift: Int = 0): (Expr, Int) = {
    val len = tokens.size
    tokens(0) match {
      case lp: TLPAREN =>
        val slice = tokens.slice(1, tokens.size)
        (parseExpression(tokens.slice(1, getPRIndex(slice) + 2), shift + 2))
      case TLET() =>
        val TLET_SHIFT = 2

        val slice = tokens.slice(TLET_SHIFT, tokens.size)
        val endBRP = getPRIndex(slice) + TLET_SHIFT + 1
        val bslice = tokens.slice(TLET_SHIFT - 1, endBRP) // Include first LPAREN
        val bettas: List[Bind] = parseBettas(bslice)
        val (e, i) = parseExpression(tokens.slice(endBRP, tokens.size), endBRP + shift)
        (ELet(bettas, e), i)
      case TSND() =>
        val (e, i) = parseExpression(tokens.slice(1, tokens.size), shift)
        (ESnd(e), i)
      case TIF() =>
        val sliceE1 = tokens.slice(1, tokens.size)
        val (e1, i1) = parseExpression(sliceE1, 1)

        val sliceE2 = tokens.slice(i1, tokens.size)
        val (e2, i2) = parseExpression(sliceE2, i1)

        val sliceE3 = tokens.slice(i2, tokens.size)
        val (e3, i3) = parseExpression(sliceE3, i2)

        (EIf(e1, e2, e3), i3 + shift)
      case TFST() =>
        val (expr, index) = parseExpression(tokens.slice(1, tokens.size), shift)
        (EFst(expr), index + 1)
      case TCONS() =>
        val sliceL = tokens.slice(1, tokens.size)
        val (left, li) = parseExpression(sliceL, 1)

        val sliceR = tokens.slice(li, tokens.size)
        val (right, ri) = parseExpression(sliceR, li)

        (ECons(left, right), ri + shift)
      case TAPP() =>
        val (name, ni) = parseExpression(tokens.slice(1, tokens.size), 1)
        val rp = getPRIndex(tokens)

        val args: List[Expr] = if (ni < tokens.size)
          parseExpressions(tokens.slice(ni, rp))
        else
          List[Expr]()
        (EApp(name, args), rp + shift)
      case tmath @ (TPLUS() | TMINUS() | TMULT() | TEQ() | TLT() | TGT()) =>
        parseMath(tmath, tokens, shift)
      case tint @ (TINT(_) | TTRUE() | TFALSE() | TNIL() | TVNAME(_)) =>
        parseSingle(tint, shift)
      case _ =>
        throw new MyParserException("I am in panick")
    }
  }

  private[this] def parseMath(tmath: Token, tokens: List[Token], shift: Int): (Expr, Int) = {
    val sliceL = tokens.slice(1, tokens.size)
    val (left, li) = parseExpression(sliceL, 1)

    val sliceR = tokens.slice(li, tokens.size)
    val (right, ri) = parseExpression(sliceR, 1)

    (tmath match {
      case TPLUS() =>
        EPlus(left, right)
      case TMINUS() =>
        EMinus(left, right)
      case TMULT() =>
        EMult(left, right)
      case TEQ() =>
        EEq(left, right)
      case TLT() =>
        ELt(left, right)
      case TGT() =>
        EGt(left, right)
    }, ri + shift + 1)
  }


  private[this] def parseSingle(token: Token, shift: Int): (Expr, Int) = {
    (
      token match {
        case TINT(n: Int) =>
          EInt(n)
        case TTRUE() =>
          ETrue()
        case TFALSE() =>
          EFalse()
        case TNIL() =>
          ENil()
        case TVNAME(str: String) =>
          EName(str)
      }, shift + 1)
  }

  def apply(tokens: List[Token]): Expr = {
    println(tokens)
//    val subExpressions: List[(Int, Int)] = getSubExpressions(tokens)
    val e = parseExpression(tokens)._1
    e
  }

  /**
    * Custom tests for parser
    * @param str
    */
  def main(str: Array[String]) = {
    { // 1
//      val code = "(fst (cons 1 2))"
//      val tokens = ProjLexer(code)
//      val (e, i) = parseExpression(tokens)
//      require(i == 8)
//      require(e.isInstanceOf[EFst])
    }

    { // 2.1
//      val code = "(val p (cons 1 (cons true nil)))"
//      val tokens = ProjLexer(code)
//      val (e, i) = parseBeta(tokens)
//
//      require(i == 13)
//      require(e.isInstanceOf[BVal])
//      require(e.isInstanceOf[Av])
    }

    { //2.2
//      val code = "(let ((val p (cons 1 (cons true nil)))) (cons 0 p))"
//      val tokens = ProjLexer(code)
//      val (e, i) = parseExpression(tokens)
//
//      require(i == 23)
//      require(e.isInstanceOf[ELet])
    }

    { //2.3
//      val code = "(let ((val p (cons 1 (cons true nil)))(val p (cons true nil))) (cons 0 p))"
//      val tokens = ProjLexer(code)
//      val (e, i) = parseExpression(tokens)
//
//      require(i == 32)
//      require(e.isInstanceOf[ELet])
//      e match {
//        case ELet(bs: List[Bind], eb: Expr) =>
//          require(bs.size == 2)
//          require(bs(0).isInstanceOf[BVal] && bs(1).isInstanceOf[BVal])
//          require(eb.isInstanceOf[ECons])
//        case _ => assert(false == true, "Test failed")
//      }
    }

    { // 3
//      val code = "(if true 10 20)"
//      val tokens = ProjLexer(code)
//      val (e, i) = parseExpression(tokens)
//
//      require(i == 6)
//      e match {
//        case EIf(a, b, c) =>
//          require(a.isInstanceOf[ETrue])
//          require(b.isInstanceOf[EInt] && b.asInstanceOf[EInt].n == 10)
//          require(c.isInstanceOf[EInt] && b.asInstanceOf[EInt].n == 20)
//        case _ => assert(false == true, "Test failed")
//      }
    }

    { // 4.1
//      val code = "(+ x y)"
//      val tokens = ProjLexer(code)
//      val (e, i) = parseExpression(tokens)
//
//      require(i == 5)
//      require(e.isInstanceOf[EPlus])
    }

    { // 4.2
//      val code = "(app f 2 3)"
//      val tokens = ProjLexer(code)
//      val (e, i) = parseExpression(tokens)
//
//      require(i == 6)
//      require(e.isInstanceOf[EApp])
    }

    { // 4.3
//      val code = "(app f 2 3 (fst (cons a (+ x y))))"
//      val tokens = ProjLexer(code)
//      val (e, i) = parseExpression(tokens)
//
//      require(i == 18)
//      e match {
//        case EApp(EName(n), es: List[Expr]) =>
//          require(n == "f")
//          require(es.size == 3)
//          require(es(0).isInstanceOf[EInt] && es(1).isInstanceOf[EInt])
//          require(es(2).isInstanceOf[EFst])
//
//          val fst = es(2).asInstanceOf[EFst]
//          require(fst.el.isInstanceOf[ECons])
//
//          val pair = fst.el.asInstanceOf[ECons]
//          require(pair.eh.isInstanceOf[EName])
//          require(pair.et.isInstanceOf[EPlus])
//      }
//      require(e.isInstanceOf[EApp])
    }

    { // 4.4. GREAT
//      val code = "(let ((def f (x (by-name y)) (+ x y))) (app f 2 3))"
//      val tokens = ProjLexer(code)
//      val (e, i) = parseExpression(tokens)
//
//      require(i == 26)
    }

    { // 5 - I even did not change anything :)
//      val code = "(let ((def g () (+ 1 2))) (let ((val f g)) f))"
//      val tokens = ProjLexer(code)
//      val (e, i) = parseExpression(tokens)
//
//      require(i == 27)
    }

    { // 6
//      val code = "(let ((val a 10) (val b (+ a 1))) (* b 3))"
//      val tokens = ProjLexer(code)
//      val (e, i) = parseExpression(tokens)
//
//      require(i == 24)
    }

    { // 7
      val code = "(let ((def f (x) (if (= x 0) 0 (+ x (app f (- x 1)))))) (let ((val g f)) (app g 5)))"
      val tokens = ProjLexer(code)
      val (e, i) = parseExpression(tokens)

      require(i == 49)
    }
  }
}
