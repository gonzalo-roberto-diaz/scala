package com.gonzalo.elevator

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Person(val weight: Double, val desiredFloor: Int)


class Elevator(val maxWeight: Double, val maxPeople: Int){

  var currentFloor: Int = 0

  private var passengers = new mutable.HashSet[Person]

  def totalWeight : Double = if (passengers.isEmpty) 0.0 else passengers.toList.map(_.weight).sum

  def totalPeople : Int = passengers.size

  private[elevator] def admit(person: Person): Boolean ={
    if ((totalWeight + person.weight) <= maxWeight && (totalPeople + 1) <= maxPeople) {
      passengers.add(person)
      true
    }else{
      false
    }
  }

  def unload(floor: Int): Set[Person] ={
    val unloaded = passengers.filter( _.desiredFloor == floor)
    passengers  --= unloaded
    return unloaded.toSet
  }

  /**
    * (the elevator) consumes a queue or persons up to its capacity in weight or amount
    *
    * @param queue   the incoming queue, which is reduced by this method
    */
  def consume(queue : mutable.Queue[Person]): Unit ={
    if (queue.isEmpty) return;

    var admitted = false
    var per = queue.front

    do{
      admitted = this.admit(per)
      if (admitted) {
        queue.dequeue()
        if (!queue.isEmpty) {
          per = queue.front
        }else{
          admitted = false
        }
      }
    }while(admitted)
  }

  /**
    * calculates the number of trips necessary to unload the current passengers. It decreases the internal set of passengers
    *
    * @return a list of tuples containint the floor and the passengers that were unloaded on that floor
    */
  def tripsForCurrentLoad(): List[Tuple2[Int, Set[Person]]]  ={
    val ret = new ListBuffer[Tuple2[Int, Set[Person]]]
    while(!passengers.isEmpty){
      val gotoFloor = closestFloor(currentFloor)
      ret += new Tuple2(gotoFloor, unload(gotoFloor))
    }
    ret.toList
  }

  private[elevator] def closestFloor(currentFloor: Int): Int  ={
    if (passengers.isEmpty) throw  new RuntimeException("cannot calculate the closest floor for an empty elevator")
    passengers.map(_.desiredFloor).reduceLeft((a, b) => if (Math.abs(a-currentFloor) < Math.abs(b-currentFloor)) a else b)
  }

}
/**
  * Created by Gonzalo on 7/4/2016.
  */
object Elevator {

    var queue = new mutable.Queue[Person]

    var trips = new mutable.ListBuffer[Tuple2[Person, Int]]


  def main(args: Array[String]) {

  }
}
