package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val takeAir = false
    val reduceMobility = false
    val vaccine = false
    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val persons: List[Person] =
    for (id <- (1 to population).toList)
      yield new Person(id).randomInfect

  val sickAfterInfected = 6
  val deadAfterInfected = 14
  val immuneAfterInfected = 16
  val healthyAfterInfected = 18

  class Person(val id: Int) {
    start

    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    val delay: Int = randomBelow(5) + 1

    //
    // to complete with simulation logic
    //
    def start(): Unit = {
      afterDelay(delay)(move)
    }

    def move(): Unit = {
      if (!dead && couldMove) {
        val dests = next
        if (!dead && !dests.isEmpty) {
          val r = randomBelow(dests.size)
          val d = dests.drop(r).head
          val pos = d._1
          row = pos._1
          col = pos._2

          setInfect(d._2)
        }
        afterDelay(delay)(move)
      }
    }

    def next = {
      val next = getNext
      next map {
        pos =>
          val list = for (
            p <- persons if pos == p.pos && !p.dead && !p.sick
          ) yield p
          (pos, list)
      }
    }

    val pos = (row, col)

    val neighbors = List(
      ((row + roomRows - 1) % roomRows, col),
      (row, (col + roomColumns - 1) % roomColumns),
      (row, (col + 1) % roomColumns),
      ((row + 1) % roomRows, col))

    def getNext = {
      if (takeAir && probability(1)) {
        val rRow = randomBelow(roomRows)
        val rCol = randomBelow(roomColumns)
        (rRow, rCol) :: neighbors
      } else neighbors
    }

    def couldMove = {
      if (reduceMobility) {
        if (infected) probability(25)
        else probability(50)
      } else true
    }

    def isVaccined = {
      if (vaccine) probability(5)
      else false
    }
    
    def randomInfect(): Person = {
      if (id <= population.toDouble * 0.01) infected = true
      this
    }

    def setInfect(ps: List[Person]): Unit = {
      def isInfect(ps: List[Person]): Boolean = {
        if (ps.isEmpty) false
        else if (ps.head.infected &&
          !infected && !immune &&
          probability(40)) true
        else isInfect(ps.tail)
      }
      if (!isVaccined) {
        if (isInfect(ps)) {
          infected = true
          afterDelay(sickAfterInfected) { sick = true }
          afterDelay(deadAfterInfected) {
            if (probability(25)) dead = true
          }
          afterDelay(immuneAfterInfected) { 
            if (!dead) immune = true }
          afterDelay(healthyAfterInfected) {
            if (!dead) {
              infected = false
              sick = false
              immune = false
            }
          }
        }
      }

    }

    def probability(i: Int): Boolean = {
      val r = randomBelow(100)
      if (r < i) true
      else false
    }

  }

}
