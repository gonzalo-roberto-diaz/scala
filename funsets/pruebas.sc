class Cake (val weight: Int, val price: Int){
  override def toString = s"wg=$weight/pr=$price"
}

class Bag(val cakes: List[Cake], val maxWeight: Int){

  def canAdd(cake: Cake): Boolean = cakes.map(_.weight).sum + cake.weight < maxWeight

  def clonePlus(cake: Cake): Bag = new Bag(cake :: cakes, maxWeight)


}

val c1=  new Cake(20, 5)
val c2=  new Cake(20, 5)
val b1 = new Bag(List(c1), 20)
b1.clonePlus(c2)






