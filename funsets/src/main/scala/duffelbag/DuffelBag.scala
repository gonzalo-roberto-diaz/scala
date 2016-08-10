package duffelbag

import scala.collection._

/**
  * Created by Gonzalo on 7/21/2016.
  */
object DuffelBag {

  class Cake (val weight: Int, val price: Int){
    override def toString = s"w=$weight/pr=$price"
  }

  class Bag(val cakes: List[Cake], val maxWeight: Int){

    def canAdd(cake: Cake): Boolean = cakes.map(_.weight).sum + cake.weight <= maxWeight

    def clonePlus(cake: Cake): Bag = new Bag(cake :: cakes, maxWeight)

    def bagPrice = cakes.map(_.price).sum

    override def toString = s"maxWeight=$maxWeight/cakes=$cakes"

  }

  var winningBag: Option[Bag] = None


  def process(bag: Bag, cakeInventory: Set[Cake] ): Unit ={

  /**
      * given a starting bag, finds out all possible "derivate" bags
      * resulting of adding another cake
      * @param orig  the starting bag
      * @param cakeCatalogue  the catalogue of possible cakes
      * @return a set of possible derivate bags
      */
    def derivateBags(orig: Bag, cakeCatalogue: Set[Cake]): Set[Bag] ={
      val cake = cakeCatalogue.head
      if (cakeCatalogue.tail.isEmpty){
        if (orig.canAdd(cake)) return Set(orig.clonePlus(cake)) else Set.empty
      }else{
        derivateBags(orig, Set(cakeCatalogue.head)) ++ derivateBags(orig, cakeCatalogue.tail)
      }
    }


    val derivates = derivateBags(bag, cakeInventory)
    derivates.foreach(bg =>{
      if (winningBag.isEmpty || bg.bagPrice > winningBag.get.bagPrice) {
        winningBag = Some(bg)
      }
      process(bg, cakeInventory)
    })

    winningBag.get
  }

  def main(args: Array[String]): Unit = {
    val cakeInventory = Set(new Cake(3, 20))
    val bag = new Bag(List.empty, 13)
    process(bag, cakeInventory)
    println(winningBag)
  }




}
