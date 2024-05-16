package ex

import util.Optionals.Optional
import util.Sequences.*

import scala.annotation.targetName
trait Item:
//  def code: Int
  def code: Sequence[Int]
  def name: String
  def tags: Sequence[String]
  override def toString: String = s"($code), \"$name\", ($tags)"//super.toString

object Item:
/*  def apply(code: Int, name: String, tags: Sequence[String] = Sequence.empty): Item =
    ItemImpl(code, name, tags)*/
  def apply(name: String, tags: Sequence[String], codes: Int*): Item =
    var cs: Sequence[Int] = Sequence.Nil()
    for c <- codes do cs = Sequence.Cons(c,cs)
    ItemImpl(cs,name,tags)
  private class ItemImpl( override val code: Sequence[Int],
                          override val name: String,
                          override val tags: Sequence[String]) extends Item;
/*
case class Item (code: Int,
 name: String,
 tags: Sequence[String]
)
*/
 /**
 * A warehouse is a place where items are stored.
 */
trait Warehouse:
  var Items: Sequence[Item];
  /**
   * Stores an item in the warehouse.
   * @param item the item to store
   */
  def store(item: Item): Unit
  /**
   * Searches for items with the given tag.
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tag: String): Sequence[Item]
  /**
   * Retrieves an item from the warehouse.
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Optional[Item]
  /**
   * Removes an item from the warehouse.
   * @param item the item to remove
   */
  def remove(item: Item): Unit
  /**
   * Checks if the warehouse contains an item with the given code.
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean
end Warehouse

object Warehouse:
  def apply(): Warehouse =
    WarehouseImpl()
private class WarehouseImpl() extends Warehouse:
    var Items: Sequence[Item] = Sequence.Nil()
    override def store(item: Item): Unit = //???
      Items = Sequence.Cons(item,Items)
    def searchItems(tag: String): Sequence[Item] = // ???
      Items.filter(_.tags.contains(tag))
    def retrieve(code: Int): Optional[Item] = //???
      Items.find(_.code.contains(code))
    def remove(item: Item): Unit = //???
      Items = Items.filter(_ != item)
    def contains(itemCode: Int): Boolean =//???
      def f1(i: Sequence[Item],c: Int): Boolean = i match
        case Sequence.Cons(h,t) if (h.code.contains(c)) =>true
        case Sequence.Cons(_,t) => f1(t,c)
        case Sequence.Nil() => false
      f1(Items,itemCode)

@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()

/*  val dellXps = Item(33,"Dell XPS 15", Sequence("notebook"))
  val dellInspiron = Item(34, "Dell Inspiron 13", Sequence("notebook"))
  val xiaomiMoped = Item(35, "Xiaomi S1", Sequence("moped", "mobility"))*/

  val dellXps = Item("Dell XPS 15", Sequence("notebook"),33,32)
  val dellInspiron = Item("Dell Inspiron 13", Sequence("notebook"),34)
  val xiaomiMoped = Item("Xiaomi S1", Sequence("moped", "mobility"),35)

//  println(warehouse.contains(dellXps.code)) // false
  println(warehouse.contains(33)) // false
  warehouse.store(dellXps) // side effect, add dell xps to the warehouse
  println(warehouse.contains(32)) // true
  warehouse.store(dellInspiron) // side effect, add dell Inspiron to the warehouse
  warehouse.store(xiaomiMoped) // side effect, add xiaomi moped to the warehouse
  println(warehouse.searchItems("mobility")) // Sequence(xiaomiMoped)
  println(warehouse.searchItems("notebook")) // Sequence(dellXps, dell Inspiron)
  println(warehouse.retrieve(11)) // None
  println(warehouse.retrieve(33)) // Just(dellXps)
  warehouse.remove(dellXps) // side effect, remove dell xps from the warehouse
  println(warehouse.retrieve(32)) // None

/** Hints:
 * - Implement the Item with a simple case class
 * - Implement the Warehouse keeping a private List of items
 * - Start implementing contains and store
 * - Implement searchItems using filter and contains
 * - Implement retrieve using find
 * - Implement remove using filter
 * - Refactor the code of Item accepting a variable number of tags (hint: use _*)
*/