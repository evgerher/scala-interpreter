package pp201802.proj
import scala.annotation.tailrec
import pp201802.proj.Data.DataBundle._

import scala.collection.immutable._
import scala.collection.mutable

object Value {

  // Environment
  implicit val level: LogLevel.Value = LogLevel.DEBUG // set NONE if do not wish to see messages
  val context: mutable.Map[String, VBind] = mutable.Map[String, VBind]()


  sealed abstract class Val

  case class VList(l: List[Val]) extends Val
  case class VPair(a: Val, b: Val) extends Val

  abstract class VValue extends Val
  case class VInt(i: Int) extends VValue
  case class VBool(b: Boolean) extends VValue
  case class VName(s: String) extends VValue
  case class VNil() extends VValue

  abstract class VBind extends Val
  case class VDef(name: String, params:List[VArg], e:Val) extends VBind
  case class VVal(x:String, e:Val) extends VBind

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
             case _ => ???
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

  def convertArg(arg: Arg): VArg = {
    logger.debug(s"Convert arg $arg")
    arg match {
      case AVname(s) => VVname(s)
      case ANname(s) => VNname(s)
      case _ => ???
    }
  }

  def convertBind(bind: Bind): VBind = {
    bind match {
      case BDef(fname, params, e) =>
        logger.debug(s"convertBind :: BDef -> VBind $bind")
        VDef(fname, params.map(convertArg), VDefBody(e))
      case BVal(x, e) =>
        logger.debug(s"convertBind :: BVal -> VVal $bind")
        VVal(x, myeval(e))
      case _ => ???
    }
  }

  def convertSingle(eh: Expr): Val = {
    eh match {
      case EInt(n) =>
        VInt(n)
      case ETrue() =>
        VBool(true)
      case EFalse() =>
        VBool(false)
      case EName(s) =>
        VName(s)
      case ENil() =>
        VNil()
    }
  }

  def convertMath(math: Expr): VValue = {
    math match {
      case EPlus(e1, e2) =>

    }
  }

  def createPair(a: Val, b: Val): Val = VPair(a, b)

  def myeval(e:Expr) : Val = {

    logger.info(s"Evaluating $e")
    e match {
      case EFst(el) => // parse pair
        logger.info(s"MYEVAL :: Get first - $e")
        el match {
          case ECons(eh, et) => // get first
            myeval(eh)
          case _ => throw new EvalException("ECons was expected...")
        }
      case ELet(bs, eb) =>
        logger.info(s"MYEVAL :: ELet -> code block $e")
        val binds: List[VBind] = bs.map(convertBind)
        binds.map{
          bind => {
            val s: String = bind match {
              case VDef(name, args, body) => name
              case VVname(name) => name
              case VNname(name) => name
            }

            (s -> bind)
          }
        }.foreach(context += _)

        createPair(VList(binds), myeval(eb))
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
      case math @ (EPlus(_, _) | EMinus(_, _) | EMult(_, _) | EGt(_, _) | ELt(_, _)) =>
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
        f match {
          case vdef: VDef =>
            logger.debug("function definition found -- OK")
          case _ =>
            logger.error("Value retrieved by name is not a function")
            throw new EvalException("Function definition expected by that name")
        }


        VInt(5)
    }
  }
}
