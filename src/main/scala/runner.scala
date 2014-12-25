package com.todesking.codevs4.runner

import Ext._

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
  var castle: CVUnit = null
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
  override def toString() =
    s"($x, $y)"
}

class Stage(
  val id: Int,
  var turn: Int = 0,
  var nextUnitID: Int = 0
) {
  val player1 = new PlayerState(1)
  val player2 = new PlayerState(2)

  def castle1: CVUnit = player1.castle
  def castle2: CVUnit = player2.castle

  val players = Seq(player1, player2)

  val field = new Field(this)

  def units: Seq[CVUnit] = field.units

  def assertInitialized(): Unit = {
    assert(castle1 != null)
    assert(castle2 != null)
  }

  def createUnit(kind: CVUnit.Kind, owner: PlayerState, pos: Pos): CVUnit = {
    registerUnit(new CVUnit(nextUnitID, kind, owner, pos))
  }

  private[this] def registerUnit[A <: CVUnit](unit: A): A = {
    unit.owner.units += unit
    unit.kind match {
      case CVUnit.Kind.Castle =>
        if(unit.owner.castle != null)
          throw new RuntimeException("城は1つしか持てない")
        unit.owner.castle = unit
      case _ =>
    }
    nextUnitID += 1
    unit
  }

  def step(p1Command: Seq[Command], p2Command: Seq[Command]): TurnResult = {
    turn += 1
    TurnResult.InProgress
  }
}

sealed abstract class Command(val unit: CVUnit) {
}
object Command {
  case class Produce(override val unit: CVUnit, kind: CVUnit.Kind) extends Command(unit)
  case class Move(override val unit: CVUnit, direction: Direction) extends Command(unit)
}

sealed abstract class TurnResult
object TurnResult {
  case object InProgress extends TurnResult
  case object P1Win extends TurnResult
  case object P2Win extends TurnResult
  case object Draw extends TurnResult
}

object Stage {
  def minimalState(stageId: Int = 0, castle1Pos: Pos = Pos(0, 0), castle2Pos: Pos = Pos(99, 99)): Stage = {
    val stage = new Stage(stageId)
    stage.createUnit(CVUnit.Kind.Castle, stage.player1, castle1Pos)
    stage.createUnit(CVUnit.Kind.Castle, stage.player2, castle2Pos)
    stage.assertInitialized()
    stage
  }
  def initialState(stageId: Int)(implicit rand: CVRandom): Stage = {
    val stage = new Stage(stageId)
    val field = stage.field

    val castle1 = stage.createUnit(CVUnit.Kind.Castle, stage.player1, field.randomPos(Pos(0, 0), 40))
    val castle2 = stage.createUnit(CVUnit.Kind.Castle, stage.player2, field.randomPos(Pos(99, 99), 40))

    (1 to 5).foreach { _ =>
      stage.createUnit(CVUnit.Kind.Worker, stage.player1, castle1.pos)
      stage.createUnit(CVUnit.Kind.Worker, stage.player2, castle2.pos)
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

  def castle1: CVUnit = stage.player1.castle
  def castle2: CVUnit = stage.player2.castle

  def units: Seq[CVUnit] = castle1.owner.units ++ castle2.owner.units

  def unitsWithin(pos: Pos, dist: Int): Seq[CVUnit] =
    units.filter { unit => pos.dist(unit.pos) <= dist }

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
  def unitsAt(pos: Pos, owner: PlayerState): Traversable[CVUnit] =
    unitsAt(pos).filter(_.owner == owner)
  def unitsAt(pos: Pos, owner: PlayerState, kind: CVUnit.Kind): Traversable[CVUnit] =
    unitsAt(pos, owner).filter(_.kind == kind)

  val resources = new ArrayBuffer[Resource]

  def addResource(pos: Pos): Unit = {
    require(!hasResourceAt(pos))
    resources += Resource(pos)
  }
}

case class Resource(pos: Pos)

class CVUnit(val id: Int, val kind: CVUnit.Kind, val owner: PlayerState, var pos: Pos) {
  def maxHp = kind.maxHp
  def attackRange = kind.attackRange
  def visibility = kind.visibility

  var hp: Int = maxHp

  def isVisible(pos: Pos): Boolean =
    this.pos.dist(pos) <= visibility

  override def toString() =
    s"CVUnit(id=${id}, owner=${owner.playerId}, pos=${pos}, kind=${kind}, HP=${hp}/${maxHp})"
}

object CVUnit {
  sealed abstract class Kind(
    val name: String,
    val code: String,
    val cost: Int,
    val attackRange: Int,
    val maxHp: Int,
    val visibility: Int,
    val creatables: Set[Kind] = Set.empty
  ) {
    def canCreate(kind: Kind): Boolean =
      creatables.contains(kind)
    override def toString() =
      s"${name}"
  }
  object Kind {
    object Worker extends Kind(
      name = "Worker",
      code = "0",
      maxHp = 2000,
      attackRange = 2,
      visibility = 10,
      cost = 40
    )
    object Castle extends Kind(
      name = "Castle",
      code = "-",
      maxHp = 50000,
      attackRange = 10,
      visibility = 10,
      cost = 0
    ) {
      override val creatables: Set[Kind] = Set(Worker)
    }
    object Knight extends Kind(
      name = "Knight",
      code = "1",
      maxHp = 5000,
      attackRange = 2,
      visibility = 4,
      cost = 20
    )
  }
}

object DamageTable {
  def apply(attacker: CVUnit.Kind, defender: CVUnit.Kind): Int = {
    import CVUnit.Kind._
    (attacker, defender) match {
      case (Worker, _) => 100
      case (Knight, Worker) => 100
      case (Knight, Knight) => 500
      case (Knight, _) => 200
      case (Castle, _) => 100
    }
  }
}

object Phase {
  object CommandPhase {
    def execute(stage: Stage, p1Command: Seq[Command], p2Command: Seq[Command]): Unit = {
      (sanitize(p1Command, stage.player1.playerId) ++ sanitize(p2Command, stage.player2.playerId)).foreach {
        case Command.Produce(unit, kind) =>
          if(unit.kind.canCreate(kind) && unit.owner.hasEnoughResource(kind.cost)) {
            stage.createUnit(kind, unit.owner, unit.pos)
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
        if(
          !commandedIds.contains(command.unit.id) &&
          command.unit.owner.playerId == playerId &&
          command.unit.hp > 0
        ) {
          commandedIds += command.unit.id
          sanitized += command
        }
      }
      sanitized
    }
  }
  object BattlePhase {
    def execute(stage: Stage): Unit = {
      stage.units.foreach { attacker =>
        val defenders =
          stage.field.unitsWithin(attacker.pos, attacker.attackRange).filter(_.owner != attacker.owner)
        val k = defenders.groupBy(_.pos).values.map { vs => Math.min(vs.size, 10) }.sum
        defenders.foreach { defender =>
          defender.hp -= DamageTable(attacker.kind, defender.kind) / k
        }
      }
    }
  }
  object SweepPhase {
    def execute(stage: Stage): Unit = {
      stage.players.foreach { player =>
        player.units.removeIf(_.hp <= 0)
      }
    }
  }
  object ResourcingPhase {
    val basicIncome = 10
    def execute(stage: Stage): Unit = {
      stage.players.foreach { player =>
        player.resources += basicIncome

        stage.field.resources.foreach { resource =>
          val workers = stage.field.unitsAt(resource.pos, owner = player, kind = CVUnit.Kind.Worker)
          player.resources += Math.min(workers.size, 5)
        }
      }
    }
  }
  object FinishingPhase {
    def result(stage: Stage): TurnResult = {
      TurnResult.InProgress
    }
  }
}
