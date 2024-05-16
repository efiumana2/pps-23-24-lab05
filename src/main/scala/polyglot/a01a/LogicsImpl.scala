package polyglot.a01a

import polyglot.a01a.Logics
import polyglot.a01a.Logics.Result
import scala.util.Random
import util.Sequences.*

case class Point(x: Int, y: Int)

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
/** Soluzione facile, imperativa e poco funzionale
class LogicsImpl(private val size: Int, private val boat: Int) extends Logics:

  private val FAILURE = 5
  private var nHit: Int = 0
  private var boatRow = 0
  private var boatLeftCol = 0
  private var failures = 0

  val random = Random()
  boatRow = random.nextInt(size)
  boatLeftCol = random.nextInt(size-boat+1)

  def hit(row: Int, col: Int):Result =
    if (row == boatRow && col >= boatLeftCol && col <= boatLeftCol+boat) {
      nHit = nHit + 1
      if (nHit == boat)
        Result.WON
      else
        Result.HIT
    } else {
      failures = failures+1
      if (failures == FAILURE)
        Result.LOST
      else
        Result.MISS
    }
 */

class LogicsImpl(private val size: Int, private val boat: Int) extends Logics:

  private val FAILURE = 5
  val X = boat
  private var boatRow = 0
  private var boatLeftCol = 0
  private var boatSeq: Sequence[Point] = Sequence.Nil()
  private var hitSeq: Sequence[Point] = Sequence.Nil()
  private var failSeq: Sequence[Point] = Sequence.Nil()

  val random = Random()
  boatRow = random.nextInt(size)
  boatLeftCol = random.nextInt(size - boat + 1)
  for i <- 0 to boat - 1 do boatSeq = Sequence.Cons(Point(boatRow,boatLeftCol+i),boatSeq)

  def hit(row: Int, col: Int):Result =
    def p = Point(row,col)
    if (boatSeq.contains(p)) {
      hitSeq = Sequence.Cons(p,hitSeq)
      hitSeq.count() match
        case X => Result.WON
        case _ => Result.HIT
    } else {
      failSeq = Sequence.Cons(p,failSeq)
      failSeq.count() match
        case FAILURE => Result.LOST
        case _ => Result.MISS
    }
