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
  case class VDef(name: String, params:List[VArg], body:VDefBody) extends VBind
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

  private[this] def convertArg(arg: Arg): VArg = {
    logger.debug(s"Convert arg $arg")
    arg match {
      case AVname(s) => VVname(s)
      case ANname(s) => VNname(s)
      case _ => ???
    }
  }

  private[this] def convertBind(bind: Bind): VBind = {
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
          case Some(v) => v
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
          val v1 = myeval(e1)
          val v2 = myeval(e2)
          executeMath(v1, v2, EPlus)
        case EMinus(e1, e2) =>
          val v1 = myeval(e1)
          val v2 = myeval(e2)
          executeMath(v1, v2, EMinus)
        case EMult(e1, e2) =>
          val v1 = myeval(e1)
          val v2 = myeval(e2)
          executeMath(v1, v2, EMult)
        case EEq(e1, e2) =>
          val v1 = myeval(e1)
          val v2 = myeval(e2)
          executeMath(v1, v2, EEq)
        case EGt(e1, e2) =>
          val v1 = myeval(e1)
          val v2 = myeval(e2)
          executeMath(v1, v2, EGt)
        case ELt(e1, e2) =>
          val v1 = myeval(e1)
          val v2 = myeval(e2)
          executeMath(v1, v2, ELt)
      }
    } catch {
      case e: RuntimeException =>
        logger.error("Unable to execute math operations, not all elements are ints")
        throw new EvalException("Expected expressions evaluable to ints")
    }
  }

  private[this] def executeMath[T >:Expr](v1: Val, v2: Val, EType: T): VValue = {
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

  private[this] def functionCall(f: VDef, vargs: List[Val]): Val = {
    // TODO: how to express `by-name` values? :: NOT IMPLEMENTED

    // should I allow it to extend?
    // Or others will just create a new one?
    val mappedArgs = mutable.Map[String, Val]()

    f.params.zipWithIndex // todo: incorrect mapping
      .map{case (vval, index) =>
        vval match {
          case VVname(s: String) => s -> vargs(index)
          case VNname(s: String) => s -> vargs(index) // todo: hz what to do
  //        case _ => index -> vval
        }
      }.foreach(mappedArgs += _) // just fucking collect into LIST!

    myeval(f.body.e)(mappedArgs.toMap)
  }

  def myeval(e: Expr)(implicit vargs: Map[String, Val] = Map()) : Val = {

//    logger.info(s"\n*** Evaluating $e ***\n")

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
              case VVal(n, e) => n
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
          case Some(vdef: VDef) =>
            logger.debug("function definition found -- OK")
          case _ =>
            logger.error("Value retrieved by name is not a function")
            throw new EvalException("Function definition expected by that name")
        }

        val vargs: List[Val] = eargs.map(myeval)
        functionCall(f.head.asInstanceOf[VDef], vargs)
    }
  }
}
