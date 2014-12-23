package com.todesking.codevs4.runner

import scala.collection.mutable.ArrayBuffer

sealed abstract class Direction
object Direction {
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction
}

class CVRandom {
  val random = new scala.util.Random

  // from(inclusive), to(inclusive)
  def nextInt(from: Int, to: Int): Int =
    from + random.nextInt(to - from + 1)
}

class PlayerState(val playerId: Int) {
  var resources: Int = 0
  var castle: Castle = null
  val units = new ArrayBuffer[CVUnit]
  def hasEnoughResource(n: Int): Boolean =
    resources >= n
  def addResource(n: Int): Unit = {
    require(n >= 0)
    resources += n
  }
  def consumeResource(n: Int): Unit = {
    require(n >= 0)
    require(hasEnoughResource(n))
    resources -= n
  }
}

case class Pos(x: Int, y: Int) {
  def dist(rhs: Pos): Int = {
    Math.abs(this.x - rhs.x) + Math.abs(this.y - rhs.y)
  }
  def move(d: Direction): Pos =
    d match {
      case Direction.Up => Pos(x, y - 1)
      case Direction.Down => Pos(x, y + 1)
      case Direction.Left => Pos(x - 1, y)
      case Direction.Right => Pos(x + 1, y)
    }
}

class Stage(
  val id: Int,
  var turn: Int = 0,
  var nextUnitID: Int = 0
) {
  val player1 = new PlayerState(1)
  val player2 = new PlayerState(2)

  def castle1: Castle = player1.castle
  def castle2: Castle = player2.castle

  val players = Seq(player1, player2)

  val field = new Field(this)

  def units: Seq[CVUnit] = field.units

  def assertInitialized(): Unit = {
    assert(castle1 != null)
    assert(castle2 != null)
  }

  def createCastle(owner: PlayerState, pos: Pos): Castle = {
    registerUnit(new Castle(nextUnitID, owner, pos))
  }

  def createWorker(owner: PlayerState, pos: Pos): Worker = {
    registerUnit(new Worker(nextUnitID, owner, pos))
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
    nextUnitID += 1
    unit
  }

  def step(p1Command: Seq[Command], p2Command: Seq[Command]): StepResult = {
    turn += 1
    StepResult.InProgress
  }
}

sealed abstract class Command(val unit: CVUnit) {
}
object Command {
  case class Produce(override val unit: CVUnit, kind: CVUnit.Kind) extends Command(unit)
  case class Move(override val unit: CVUnit, direction: Direction) extends Command(unit)
}

sealed abstract class StepResult
object StepResult {
  case object InProgress extends StepResult
  case object P1Win extends StepResult
  case object P2Win extends StepResult
  case object Draw extends StepResult
}

object Stage {
  def minimalState(stageId: Int = 0, castle1Pos: Pos = Pos(0, 0), castle2Pos: Pos = Pos(99, 99)): Stage = {
    val stage = new Stage(stageId)
    stage.createCastle(stage.player1, castle1Pos)
    stage.createCastle(stage.player2, castle2Pos)
    stage.assertInitialized()
    stage
  }
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
      field.addResource(field.randomPosThat(Pos(0, 0), 99)(resourceCond))
      field.addResource(field.randomPosThat(Pos(99, 99), 99)(resourceCond))
    }

    stage.assertInitialized()
    stage
  }
}

class Field(val stage: Stage) {
  val width: Int = 100
  val height: Int = 100

  def castle1: Castle = stage.player1.castle
  def castle2: Castle = stage.player2.castle

  def units: Seq[CVUnit] = castle1.owner.units ++ castle2.owner.units

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

  def validPos(pos: Pos): Boolean =
    0 <= pos.x && pos.x < width && 0 <= pos.y && pos.y < height

  def hasResourceAt(pos: Pos): Boolean = {
    resources.exists(_.pos == pos)
  }

  def unitsAt(pos: Pos): Seq[CVUnit] =
    units.filter(_.pos == pos)

  val resources = new ArrayBuffer[Resource]

  def addResource(pos: Pos): Unit = {
    require(!hasResourceAt(pos))
    resources += Resource(pos)
  }
}

case class Resource(pos: Pos)

sealed abstract class CVUnit(val id: Int, val owner: PlayerState, var pos: Pos) {
  def maxHp: Int
  val kind: CVUnit.Kind
  var hp: Int = maxHp
  def visibility: Int
  def isVisible(pos: Pos): Boolean =
    this.pos.dist(pos) <= visibility
}

object CVUnit {
  sealed abstract class Kind(val code: String, val cost: Int) {
    def create(stage: Stage, owner: PlayerState, pos: Pos): CVUnit
    def canCreate(kind: Kind): Boolean =
      creatables.contains(kind)
    val creatables: Set[Kind] = Set.empty
  }
  object Kind {
    object Worker extends Kind("0", 40) {
      override def create(stage: Stage, owner: PlayerState, pos: Pos) =
        stage.createWorker(owner, pos)
    }
    object Castle extends Kind("-", 0) {
      override def create(stage: Stage, owner: PlayerState, pos: Pos) =
        stage.createCastle(owner, pos)
      override val creatables: Set[Kind] = Set(Worker)
    }
    // TODO: more
  }
}

class Castle(id: Int, owner: PlayerState, pos: Pos) extends CVUnit(id, owner, pos) {
  override lazy val kind = CVUnit.Kind.Castle
  override lazy val maxHp = 50000
  override lazy val visibility = 10
}

class Worker(id: Int, owner: PlayerState, pos: Pos) extends CVUnit(id, owner, pos) {
  override lazy val kind = CVUnit.Kind.Worker
  override lazy val maxHp = 2000
  override lazy val visibility = 10
}

object Phase {
  object CommandPhase {
    def execute(stage: Stage, p1Command: Seq[Command], p2Command: Seq[Command]): Unit = {
      (sanitize(p1Command, stage.player1.playerId) ++ sanitize(p2Command, stage.player2.playerId)).foreach {
        case Command.Produce(unit, kind) =>
          if(unit.kind.canCreate(kind) && unit.owner.hasEnoughResource(kind.cost)) {
            kind.create(stage, unit.owner, unit.pos)
            unit.owner.consumeResource(kind.cost)
          }
        case Command.Move(unit, direction) =>
          val newPos = unit.pos.move(direction)
          if(stage.field.validPos(newPos)) {
            unit.pos = newPos
          }
      }
    }
    private[this] def sanitize(commands: Seq[Command], playerId: Int): Seq[Command] = {
      val commandedIds = scala.collection.mutable.HashSet.empty[Int]
      val sanitized = new ArrayBuffer[Command]
      commands.foreach { command =>
        if(!commandedIds.contains(command.unit.id) && command.unit.owner.playerId == playerId) {
          commandedIds += command.unit.id
          sanitized += command
        }
      }
      sanitized
    }
  }
}
