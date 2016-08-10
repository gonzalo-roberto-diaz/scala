package com.gonzalo.elevator


import org.scalatest.FlatSpec

import scala.collection.mutable

/**
  * Created by Gonzalo on 7/4/2016.
  */
class ElevatorSuite extends FlatSpec{

  "An elevator " should " not allow people beyond the weight limit " in{
    val elevator = new Elevator(200.0, 3)

    val person1 = new Person(60.0,3)
    assert(elevator.admit(person1))

    val person2 = new Person(100.0,5)
    assert(elevator.admit(person2))

    val person3 = new Person(50.0,4)
    assert(!elevator.admit(person3))
  }

  it should "not allow more people than the limit " in {
    val elevator = new Elevator(200.0, 2)

    val person1 = new Person(40.0,2)
    assert(elevator.admit(person1))

    val person2 = new Person(50.0,5)
    assert(elevator.admit(person2))

    val person3 = new Person(50.0,4)
    assert(!elevator.admit(person3))
  }

  it should "unload the proper people according to the floor " in {
    val elevator = new Elevator(400.0, 3)
    elevator.admit(new Person(40.0, 2))
    elevator.admit(new Person(50.0, 2))
    elevator.admit(new Person(70.0, 3))

    elevator.unload(1)
    assert(elevator.totalPeople == 3)

    elevator.unload(2)
    assert(elevator.totalPeople == 1)

  }

  it should "comsume a queue according to maximum weight" in {
    val elevator = new Elevator(200.0, 10)
    val queue = new mutable.Queue[Person]
    queue.enqueue(new Person(50.0, 2))
    queue.enqueue(new Person(50.0, 3))
    queue.enqueue(new Person(100.0, 2))
    queue.enqueue(new Person(40.0, 2))
    elevator.consume(queue)
    assert(queue.size == 1 )
  }

  it should "comsume a queue according to maximum number of passengers" in {
    val elevator = new Elevator(200.0, 3)
    val queue = new mutable.Queue[Person]
    queue.enqueue(new Person(40.0, 2))
    queue.enqueue(new Person(40.0, 3))
    queue.enqueue(new Person(40.0, 2))
    queue.enqueue(new Person(40.0, 5))
    elevator.consume(queue)
    assert(queue.size == 1 )
  }

  it should "correctly pick the closest floor" in {
    val elevator = new Elevator(1000.0, 10)
    val queue = new mutable.Queue[Person]
    queue.enqueue(new Person(40.0, 1))
    queue.enqueue(new Person(40.0, 4))
    queue.enqueue(new Person(40.0, 4))
    queue.enqueue(new Person(40.0, 5))
    queue.enqueue(new Person(40.0, 6))
    elevator.consume(queue)
    elevator.currentFloor = 3
    assert(elevator.closestFloor(3) == 4)
  }

  it should "make the right amount of trips according to its floor and load" in {
    val elevator = new Elevator(1000.0, 10)
    val queue = new mutable.Queue[Person]
    queue.enqueue(new Person(40.0, 4))
    queue.enqueue(new Person(40.0, 4))
    queue.enqueue(new Person(40.0, 5))
    queue.enqueue(new Person(40.0, 6))
    elevator.consume(queue)
    elevator.currentFloor = 3
    val trips =  elevator.tripsForCurrentLoad()

    assert(trips.size == 3)

    assert(trips(0)._1 == 4 )
    assert(trips(0)._2.size == 2 )

    assert(trips(1)._1 == 5 )
    assert(trips(1)._2.size == 1 )

    assert(trips(2)._1 == 6 )
    assert(trips(2)._2.size ==  1 )
  }





}
