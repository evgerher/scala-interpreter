package pp201802.proj
import scala.annotation.tailrec
import pp201802.proj.Data.DataBundle._
import scala.collection.immutable._

object Value {

  // Environment

  sealed abstract class Val
  
  abstract class ConvertToScala[A] {
    def toInt(a:A) : Option[Int]
    def toBool(a:A) : Option[Boolean]
    def toPair(a:A) : Option[(A, A)]
    def isNil(a:A) : Boolean
    def isDef(a:A) : Boolean
    def isRec(a:A) : Boolean
  }

   implicit val valConv : ConvertToScala[Val] = new ConvertToScala[Val] {
     override def toInt(a: Val): Option[Int] = ???

     override def toBool(a: Val): Option[Boolean] = ???

     override def toPair(a: Val): Option[(Val, Val)] = ???

     override def isNil(a: Val): Boolean = ???

     override def isDef(a: Val): Boolean = ???

     override def isRec(a: Val): Boolean = ???
   }
}

object Main {
  import Value._

  class EvalException(val msg: String) extends Exception

  def myeval(e:Expr) : Val = throw new EvalException("Not implemented yet")

}