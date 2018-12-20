package pp201802.proj
import scala.annotation.tailrec
import pp201802.proj.Data.DataBundle._

import scala.collection.immutable._
import scala.collection.mutable

object Value {

  // Environment
  implicit val level: LogLevel.Value = LogLevel.DEBUG // set NONE if do not wish to see messages
  val context: mutable.Map[String, Val] = mutable.Map[String, Val]()


  sealed abstract class Val

  case class VList(l: List[Val]) extends Val
  case class VPair(a: Val, b: Val) extends Val

  abstract class VValue extends Val
  case class VInt(i: Int) extends VValue
  case class VBool(b: Boolean) extends VValue
  case class VName(s: String) extends VValue
  case class VNil() extends VValue

  abstract class VBind extends Val
  case class VExecuteLater(e: Expr) extends Val

  case class VDef(name: String, params:List[VArg], body:VDefBody) extends VBind
  case class VVal(x:String, bind:Val) extends VBind

  abstract class VArg extends Val
  case class VVname(s: String) extends VArg
  case class VNname(s: String) extends VArg

  case class VDefBody(e: Expr) extends Val

  abstract class ConvertToScala[A] {
    def toInt(a:A) : Option[Int]
    def toBool(a:A) : Option[Boolean]
    def toPair(a:A) : Option[(A, A)]
    def isNil(a:A) : Boolean
    def isDef(a:A) : Boolean
    def isRec(a:A) : Boolean
  }

   implicit val valConv : ConvertToScala[Val] = new ConvertToScala[Val] {
     override def toInt(a: Val): Option[Int] = {
       a match {
         case v: VValue =>
           v match {
             case VInt(i) => Some(i)
             case VBool(b) => Some(if (b) 1 else 0)
             case _ => None
           }
         case _ => ???
       }
     }

     override def toBool(a: Val): Option[Boolean] = {
       a match {
         case v: VValue =>
           v match {
             case VBool(b) => Some(b)
             case _ => None
           }
         case _ => ???
       }
     }

     override def toPair(a: Val): Option[(Val, Val)] = {
       a match {
         case VPair(a, b) =>
           Some(a, b)
         case _ => ???
       }
     }

     override def isNil(a: Val): Boolean = {
       a match {
         case VNil() =>
           true
         case _ => ???
       }
     }

     override def isDef(a: Val): Boolean = {
       a match {
         case bind: VBind =>
           bind match {
             case VDef(name, args, e) =>
               true
             case VVal(name: String, bind: VBind) =>
               return isDef(bind)
           }

         case _ => ???
       }
     }

     override def isRec(a: Val): Boolean = ???
   }
}

object LogLevel extends Enumeration {
  val NONE, ERROR, INFO, DEBUG = Value
}

// As there is no logger and I do not want to add dependency on slf4j/log4j
// I would define one myself
object logger {
  def info(s: Any)(implicit level: LogLevel.Value): Unit = {
    apply(s"[INFO] $s", level >= LogLevel.INFO)
  }

  def debug(s: Any)(implicit level: LogLevel.Value): Unit = {
    apply(s"[DEBUG] $s", level >= LogLevel.DEBUG) // todo: how does it work?
  }

  def error(s: Any)(implicit level: LogLevel.Value): Unit = {
    apply(s"[ERROR] $s", level >= LogLevel.ERROR)
  }

  def apply(s: Any, boolean: Boolean): Unit = {
    if (boolean)
      println(s.toString)
  }
}

object Main {
  import Value._

  class EvalException(val msg: String) extends Exception

  private[this] def convertArg(arg: Arg): VArg = {
    logger.debug(s"Convert arg $arg")
    arg match {
      case AVname(s) => VVname(s)
      case ANname(s) => VNname(s)
      case _ => ???
    }
  }

  private[this] def convertBind(bind: Bind)(implicit vargs: Map[String, Val]): VBind = {
    bind match {
      case BDef(fname, params, e) =>
        logger.debug(s"convertBind :: BDef -> VBind $bind")
        VDef(fname, params.map(convertArg), VDefBody(e))
      case BVal(x, e) =>
        logger.debug(s"convertBind :: BVal -> VVal $bind")
        VVal(x, myeval(e))
      case BLval(x, e) =>
        logger.debug(s"convertBind :: BLval -> VVal $bind")
        VVal(x, VExecuteLater(e))
      case _ => ???
    }
  }

  private[this] def convertSingle(eh: Expr)(implicit vargs: Map[String, Val]) : Val = {
    eh match {
      case EInt(n) =>
        VInt(n)
      case ETrue() =>
        VBool(true)
      case EFalse() =>
        VBool(false)
      case EName(s) =>
        vargs.get(s) match {
          case Some(v) => v match {
            case VVal(s: String, later: VExecuteLater) =>
              myeval(later.e)
            case VVal(s: String, other: Val) =>
              other
            case _ => v
          }
          case _ => VName(s)
        }
      case ENil() =>
        VNil()
    }
  }

  private[this] def convertMath(math: Expr)(implicit vargs: Map[String, Val]): VValue = {
    logger.debug(s"convertMath :: convert $math")
    try {
      // Yes, looks terrible, but all of them do not have one ancestor with two params...
      math match {
        case EPlus(e1, e2)  =>
          val v1 = readParam(e1)
          val v2 = readParam(e2)
          executeMath(v1, v2, math)
        case EMinus(e1, e2) =>
          val v1 = readParam(e1)
          val v2 = readParam(e2)
          executeMath(v1, v2, math)
        case EMult(e1, e2) =>
          val v1 = readParam(e1)
          val v2 = readParam(e2)
          executeMath(v1, v2, math)
        case EEq(e1, e2) =>
          val v1 = readParam(e1)
          val v2 = readParam(e2)
          executeMath(v1, v2, math)
        case EGt(e1, e2) =>
          val v1 = readParam(e1)
          val v2 = readParam(e2)
          executeMath(v1, v2, math)
        case ELt(e1, e2) =>
          val v1 = readParam(e1)
          val v2 = readParam(e2)
          executeMath(v1, v2, math)
      }
    } catch {
      case e: RuntimeException =>
        logger.error("Unable to execute math operations, not all elements are ints")
        throw new EvalException("Expected expressions evaluable to ints")
    }
  }

  private[this] def readParam(expr: Expr)(implicit vargs: Map[String, Val]): VInt = {
    val v: Val = myeval(expr)
    v match {
      case VInt(n) => v.asInstanceOf[VInt]
      case VVal(a: String, vint: VInt) => vint
      case VVal(a: String, later: VExecuteLater) => readParam(later.e)
      case _ => ???
    }
  }

  private[this] def executeMath(v1: Val, v2: Val, EType: Expr): VValue = {
    val (i1, i2): (Int, Int) = (v1, v2) match {
      case (VInt(a), VInt(b)) =>
        (a, b)
      case _ => throw new RuntimeException("Not all elements are ints")
    }

    logger.debug(s"executeMath :: values are $i1, $i2")

    // todo: should I worry about TRUE == TRUE ?

    EType match {
      case minus: EMinus =>
        VInt(i1 - i2)
      case plus: EPlus =>
        VInt(i1 + i2)
      case mult: EMult =>
        VInt(i1 * i2)
      case eq: EEq =>
        VBool(i1 == i2)
      case gt: EGt =>
        VBool(i1 > i2)
      case lt: ELt =>
        VBool(i1 < i2)
    }
  }

  private[this] def createPair(a: Val, b: Val): Val = VPair(a, b)

  private[this] def functionCall(f: VDef, args: List[Val])(implicit vargs: Map[String, Val]): Val = {
    // TODO: how to express `by-name` values? :: NOT IMPLEMENTED

    // Override old variables if needed
    val mappedArgs = mutable.Map[String, Val]()
    mappedArgs ++= vargs

    f.params.zipWithIndex // todo: incorrect mapping
      .map{case (vval, index) =>
        vval match {
          case VVname(s: String) => s -> args(index)
          case VNname(s: String) => s -> args(index) // todo: hz what to do
  //        case _ => index -> vval
        }
      }.foreach(mappedArgs += _) // just fucking collect into LIST!

    myeval(f.body.e)(mappedArgs.toMap)
  }

  private[this] def parseMatch(em: EMatch)(implicit vargs: Map[String, Val]): Val = {
    val v1 = myeval(em.e1)
    v1 match {
      case VPair(a, b) => // Evaluate E3 with bindings if Pair
        // Add new context items
        val modifiedContext = mutable.Map[String, Val]()
        modifiedContext ++= vargs
        modifiedContext += (em.hd -> VVal(em.hd, a))
        modifiedContext += (em.tl -> VVal(em.tl, b))

        // Evaluate E3
        myeval(em.e3)(modifiedContext.toMap)
      case VNil() => // Evaluate E2 if Nil
        myeval(em.e2)
      case _ =>
        v1
    }
  }

  def myeval(e: Expr)(implicit vargs: Map[String, Val] = Map()) : Val = {

//    logger.info(s"\n*** Evaluating $e ***\n")

    e match {
      case m: EMatch =>
        parseMatch(m)
      case EFst(el) => // parse pair
        logger.info(s"MYEVAL :: Get first - $e")
        el match {
          case ECons(eh, et) => // get first
            myeval(eh)
          case _ => throw new EvalException("ECons was expected...")
        }
      case ELet(bs, eb) =>
        logger.info(s"MYEVAL :: ELet -> code block $e")
        bs.foreach(b => {
          val converted = convertBind(b)(context.toMap)
          val s: String = converted match {
            case VDef(name, args, body) => name
            case VVal(s, e) => s
          }
          context += (s -> converted)
        })

        myeval(eb)(context.toMap)
      case ECons(eh, et) =>
        createPair(myeval(eh), myeval(et))
      case single @ (EInt(_) | EName(_) | ETrue() | EFalse() | ENil()) =>
        logger.info(s"MYEVAL :: Convert single ${single}")
        convertSingle(single)
      case EIf(econd, et, ef) =>
        logger.info(s"MYEVAL :: Convert EIF, check whether `econd` is bool. $econd")
        val bool: Val = myeval(econd)
        bool match {
          case VBool(b) =>
            if (b) myeval(et) else myeval(ef)
          case _ =>
            logger.error("Unable to determine boolean from expression")
            throw new EvalException("Expected VBool")
        }
      case math @ (EPlus(_, _) | EMinus(_, _) | EMult(_, _) | EGt(_, _) | ELt(_, _) | EEq(_, _)) =>
        convertMath(math)
      case EApp(ef, eargs) =>
        val name = ef match {
          case EName(s) =>
            s
          case _ =>
            logger.error("Unable to retrieve a function name")
            throw new EvalException("Function name expected")
        }

        val f = context.get(name)
        // Expected that it is not possible to name a variable the same way as a function
        val vdef = f match {
          case Some(vdef: VDef) =>
            logger.debug("function definition found -- OK")
            vdef
          case Some(VVal(s: String, vdef: VDef)) =>
            logger.debug("function definition found -- OK")
            vdef
          case _ =>
            logger.error("Value retrieved by name is not a function")
            throw new EvalException("Function definition expected by that name")
        }

        val args: List[Val] = eargs.map(myeval)
        functionCall(vdef, args)
    }
  }
}
