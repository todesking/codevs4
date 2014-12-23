package com.todesking.codevs4.runner

import scala.collection.mutable.ArrayBuffer

class CVRandom {
  val random = new scala.util.Random

  // from(inclusive), to(inclusive)
  def nextInt(from: Int, to: Int): Int =
    from + random.nextInt(to - from + 1)
}

class PlayerState {
  var resources: Int = 0
  var castle: Castle = null
  val units = new ArrayBuffer[CVUnit]
}

case class Pos(x: Int, y: Int) {
  def dist(rhs: Pos): Int = {
    Math.abs(this.x - rhs.x) + Math.abs(this.y - rhs.y)
  }
}

class Stage(
  val id: Int,
  var turn: Int = 0,
  var nextUnitID: Int = 12
) {
  val player1 = new PlayerState()
  val player2 = new PlayerState()
  val players = Seq(player1, player2)
  val field = new Field(this)

  def createCastle(owner: PlayerState, pos: Pos): Castle = {
    registerUnit(Castle(owner, pos))
  }

  def createWorker(owner: PlayerState, pos: Pos): Worker = {
    registerUnit(Worker(owner, pos))
  }

  private[this] def registerUnit[A <: CVUnit](unit: A): A = {
    unit.owner.units += unit
    unit match {
      case c: Castle =>
        if(unit.owner.castle != null)
          throw new RuntimeException("城は1つしか持てない")
        unit.owner.castle = c
      case _ =>
    }
    unit
  }

  def step(p1Command: Seq[Command], p2Command: Seq[Command]): StepResult = {
    turn += 1
    StepResult.InProgress
  }
}

sealed class Command
object Command {
  object Nop extends Command
}

sealed class StepResult
object StepResult {
  case object InProgress extends StepResult
  case object P1Win extends StepResult
  case object P2Win extends StepResult
  case object Draw extends StepResult
}

object Stage {
  def initialState(stageId: Int)(implicit rand: CVRandom): Stage = {
    val stage = new Stage(stageId)
    val field = stage.field

    val castle1 = stage.createCastle(stage.player1, field.randomPos(Pos(0, 0), 40))
    val castle2 = stage.createCastle(stage.player2, field.randomPos(Pos(99, 99), 40))

    (1 to 5).foreach { _ =>
      stage.createWorker(stage.player1, castle1.pos)
      stage.createWorker(stage.player2, castle2.pos)
    }

    val resourceCond: Pos => Boolean = {pos =>
      !field.hasResourceAt(pos) &&
        !field.castle1.isVisible(pos) &&
        !field.castle2.isVisible(pos)
    }
    (1 to 10).foreach { _ =>
      field.addResource(field.randomPosThat(Pos(0, 0), 98)(resourceCond))
      field.addResource(field.randomPosThat(Pos(99, 99), 98)(resourceCond))
    }

    stage
  }
}

class Field(val stage: Stage) {
  val width: Int = 100
  val height: Int = 100

  def castle1: Castle = stage.player1.castle
  def castle2: Castle = stage.player2.castle

  def randomPos(center: Pos, dist: Int)(implicit rand: CVRandom): Pos = {
    val x = rand.nextInt(Math.max(center.x - dist, 0), Math.min(center.x + dist, width - 1))
    val d = dist - Math.abs(x - center.x)
    val y = rand.nextInt(Math.max(center.y - d, 0), Math.min(center.x + d, height - 1))
    Pos(x, y)
  }

  def randomPosThat(center: Pos, dist: Int)(that: Pos => Boolean)(implicit rand: CVRandom): Pos = {
    var pos = randomPos(center, dist)
    while(!that(pos))
      pos = randomPos(center, dist)
    pos
  }

  def hasResourceAt(pos: Pos): Boolean = {
    resources.exists(_.pos == pos)
  }

  val resources = new ArrayBuffer[Resource]

  def addResource(pos: Pos): Unit = {
    require(!hasResourceAt(pos))
    resources += Resource(pos)
  }
}

case class Resource(pos: Pos)

sealed abstract class CVUnit(val owner: PlayerState, val pos: Pos) {
  def maxHp: Int
  var hp: Int = maxHp
  def visibility: Int
  def isVisible(pos: Pos): Boolean =
    this.pos.dist(pos) <= visibility
}

case class Castle(override val owner: PlayerState, override val pos: Pos) extends CVUnit(owner, pos) {
  override lazy val maxHp = 50000
  override lazy val visibility = 10
}

case class Worker(override val owner: PlayerState, override val pos: Pos) extends CVUnit(owner, pos) {
  override lazy val maxHp = 2000
  override lazy val visibility = 10
}


