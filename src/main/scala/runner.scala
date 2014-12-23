package com.todesking.codevs4.runner

class CVRandom {
  val random = new scala.util.Random

  // from(inclusive), to(inclusive)
  def nextInt(from: Int, to: Int): Int =
    from + random.nextInt(to - from + 1)
}

class PlayerState {
  var resources: Int = 0
  val units = new scala.collection.mutable.ArrayBuffer[CVUnit]
}

case class Pos(x: Int, y: Int)

class Stage(
  val id: Int,
  var turn: Int = 0,
  var nextUnitID: Int = 12
) {
  val player1 = new PlayerState()
  val player2 = new PlayerState()
  val players = Seq(player1, player2)
  val field = new Field()

  def createCastle(owner: PlayerState, pos: Pos): Castle = {
    registerUnit(Castle(owner, pos))
  }

  def createWorker(owner: PlayerState, pos: Pos): Worker = {
    registerUnit(Worker(owner, pos))
  }

  private[this] def registerUnit[A <: CVUnit](unit: A): A = {
    unit.owner.units += unit
    unit
  }
}

object Stage {
  def initialState(stageId: Int)(implicit rand: CVRandom): Stage = {
    val stage = new Stage(stageId)

    val castle1 = stage.createCastle(stage.player1, stage.field.randomPos(Pos(0, 0), 99))
    val castle2 = stage.createCastle(stage.player2, stage.field.randomPos(Pos(99, 99), 99))

    (1 to 5).foreach { _ =>
      stage.createWorker(stage.player1, castle1.pos)
      stage.createWorker(stage.player2, castle2.pos)
    }

    stage
  }
}

class Field {
  val width: Int = 100
  val height: Int = 100

  def randomPos(center: Pos, dist: Int)(implicit rand: CVRandom): Pos = {
    Pos(
      rand.nextInt(Math.max(center.x - dist, 0), Math.min(center.x + dist, width - 1)),
      rand.nextInt(Math.max(center.y - dist, 0), Math.min(center.x + dist, height - 1))
    )
  }
}

sealed abstract class CVUnit(val owner: PlayerState, pos: Pos) {
}

case class Castle(override val owner: PlayerState, pos: Pos) extends CVUnit(owner, pos) {
}

case class Worker(override val owner: PlayerState, pos: Pos) extends CVUnit(owner, pos) {
}


