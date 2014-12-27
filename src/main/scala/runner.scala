package com.todesking.codevs4.runner

import com.todesking.codevs4.interface._

import Ext._

import scala.collection.mutable.ArrayBuffer

trait Logger {
  def turnStart(stage: Stage, commands1: Seq[Command], commands2: Seq[Command]): Unit = ()
  def commandIgnored(playerId: Int, command: Command, message: String): Unit = ()
  def stageEnd(stage: Stage, result: TurnResult): Unit = ()
}
object Logger {
  val Null = new Logger {
  }
  val ShowError = new Logger {
    override def commandIgnored(playerId: Int, command: Command, message: String): Unit = {
      println(s"Player ${playerId}: command ignored: ${command}, message=${message}")
    }
  }
  val ShowAll = new Logger {
    override def turnStart(stage: Stage, commands1: Seq[Command], commands2: Seq[Command]): Unit = {
      val h = "[INFO]"
      println(s"$h STAGE ${stage.id}, Turn ${stage.turn}")
      stage.players.zip(Seq(commands1, commands2)).foreach { case (p, commands) =>
        val unitsCount = Seq(CVUnitKind.Worker, CVUnitKind.Knight, CVUnitKind.Fighter, CVUnitKind.Assassin).map { kind =>
          p.units.filter(_.kind == kind).size
        }.mkString("/")
        println(s"$h   Player ${p.playerId}, Resources ${p.resources}, Castle ${p.castle.hpString}, Units(W/K/F/A) ${unitsCount}")
        commands.foreach { c =>
          println(s"$h     ${c}")
        }
      }
    }
    override def commandIgnored(playerId: Int, command: Command, message: String): Unit = {
      println(s"[WARN] Player ${playerId}: command ignored: ${command}, message=${message}")
    }
    override def stageEnd(stage: Stage, result: TurnResult): Unit = {
      println(s"[INFO] Stage ${stage.id} end: ${result}, Turn ${stage.turn}")
    }
  }
}

class PlayerState(val playerId: Int, val coordinateSystem: CoordinateSystem) {
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
  def visibilities(): Traversable[(Pos, Int)] =
    units.groupBy(_.pos).map { case (pos, units) => (pos, units.map(_.visibility).max) }
}

class Stage(
  val id: Int,
  var turn: Int = 0,
  var nextUnitID: Int = 0
) {
  val player1 = new PlayerState(1, CoordinateSystem.TopLeft)
  val player2 = new PlayerState(2, CoordinateSystem.BottomRight)

  def castle1: CVUnit = player1.castle
  def castle2: CVUnit = player2.castle

  val players = Seq(player1, player2)

  val field = new Field(this)

  def units: Seq[CVUnit] = field.units

  def unit(id: Int): CVUnit = units.filter(_.id == id).head

  def assertInitialized(): Unit = {
    assert(castle1 != null)
    assert(castle2 != null)
  }

  def createUnit(kind: CVUnitKind, owner: PlayerState, pos: Pos): CVUnit = {
    registerUnit(new CVUnit(nextUnitID, kind, owner, pos))
  }

  private[this] def registerUnit[A <: CVUnit](unit: A): A = {
    unit.owner.units += unit
    unit.kind match {
      case CVUnitKind.Castle =>
        if(unit.owner.castle != null)
          throw new RuntimeException("城は1つしか持てない")
        unit.owner.castle = unit
      case _ =>
    }
    nextUnitID += 1
    unit
  }

  def executeTurn(p1Command: Seq[Command], p2Command: Seq[Command])(implicit logger: Logger): TurnResult = {
    logger.turnStart(this, p1Command, p2Command)
    Phase.CommandPhase.execute(this, p1Command, p2Command)
    Phase.BattlePhase.execute(this)
    Phase.SweepPhase.execute(this)
    Phase.ResourcingPhase.execute(this)
    val result = Phase.FinishingPhase.result(this)
    turn += 1
    result
  }

  def visibleStateFor(playerId: Int): VisibleState = {
    val player = players.filter(_.playerId == playerId).head
    val opponent = players.filter(_.playerId != playerId).head
    VisibleState(
      stageId = this.id,
      turn = this.turn,
      resources = player.resources,
      playerUnits = player.units.map { u => u.toView.copy(pos = player.coordinateSystem.toLocal(u.pos)) },
      opponentUnits = visibleUnits(player, opponent).map { u => u.toView.copy(pos = player.coordinateSystem.toLocal(u.pos)) },
      resourceLocations = visibleResources(player).map { pos => player.coordinateSystem.toLocal(pos) }.toSeq
    )
  }

  def visibleUnits(viewer: PlayerState, owner: PlayerState): Seq[CVUnit] = {
    viewer.visibilities.
      flatMap { case (pos, visibility) => field.unitsWithin(pos, visibility, owner) }.
      toSet.toSeq.sortBy[Int](_.id)
  }
  def visibleResources(viewer: PlayerState): Traversable[Pos] = {
    viewer.visibilities.flatMap { case (pos, visibility) =>
      field.resources.filter { r => pos.ranged(visibility).contains(r.pos) }.map(_.pos)
    }.toSet
  }
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
    stage.createUnit(CVUnitKind.Castle, stage.player1, castle1Pos)
    stage.createUnit(CVUnitKind.Castle, stage.player2, castle2Pos)
    stage.assertInitialized()
    stage
  }
  def initialState(stageId: Int)(implicit rand: RandomSource): Stage = {
    val stage = new Stage(stageId)
    val field = stage.field

    val castle1 = stage.createUnit(CVUnitKind.Castle, stage.player1, field.randomPos(Pos(0, 0), 40))
    val castle2 = stage.createUnit(CVUnitKind.Castle, stage.player2, field.randomPos(Pos(99, 99), 40))

    (1 to 5).foreach { _ =>
      stage.createUnit(CVUnitKind.Worker, stage.player1, castle1.pos)
      stage.createUnit(CVUnitKind.Worker, stage.player2, castle2.pos)
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
    units.filter { unit => pos.ranged(dist).contains(unit.pos) }
  def unitsWithin(pos: Pos, dist: Int, owner: PlayerState): Seq[CVUnit] =
    unitsWithin(pos, dist).filter(_.owner == owner)

  def randomPos(center: Pos, dist: Int)(implicit rand: RandomSource): Pos = {
    val x = rand.nextInt(Math.max(center.x - dist, 0), Math.min(center.x + dist, width - 1))
    val d = dist - Math.abs(x - center.x)
    val y = rand.nextInt(Math.max(center.y - d, 0), Math.min(center.x + d, height - 1))
    Pos(x, y)
  }

  def randomPosThat(center: Pos, dist: Int)(that: Pos => Boolean)(implicit rand: RandomSource): Pos = {
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
  def unitsAt(pos: Pos, owner: PlayerState, kind: CVUnitKind): Traversable[CVUnit] =
    unitsAt(pos, owner).filter(_.kind == kind)

  val resources = new ArrayBuffer[Resource]

  def addResource(pos: Pos): Unit = {
    require(!hasResourceAt(pos))
    resources += Resource(pos)
  }
}

case class Resource(pos: Pos)

object Phase {
  object CommandPhase {
    def execute(stage: Stage, p1Command: Seq[Command], p2Command: Seq[Command])(implicit log: Logger): Unit = {
      (sanitize(stage, p1Command, stage.player1.playerId) ++ sanitize(stage, p2Command, stage.player2.playerId)).foreach {
        case command@Command.Produce(unitId, kind) =>
          val unit = stage.unit(unitId)
          if(!unit.kind.canCreate(kind)) {
            log.commandIgnored(unit.owner.playerId, command, s"${unit.kind.name} can't produce ${kind.name}")
          } else if(!unit.owner.hasEnoughResource(kind.cost)) {
            log.commandIgnored(unit.owner.playerId, command, s"to produce ${kind.name} need ${kind.cost} resources but only ${unit.owner.resources} available")
          } else {
            stage.createUnit(kind, unit.owner, unit.pos)
            unit.owner.consumeResource(kind.cost)
          }
        case command@Command.Move(unitId, direction) =>
          val unit = stage.unit(unitId)
          val newPos = unit.pos.move(unit.owner.coordinateSystem.toGlobal(direction))
          if(stage.field.validPos(newPos) && unit.movable()) {
            unit.pos = newPos
          }
      }
    }
    private[this] def sanitize(stage: Stage, commands: Seq[Command], playerId: Int): Seq[Command] = {
      val commandedIds = scala.collection.mutable.HashSet.empty[Int]
      val sanitized = new ArrayBuffer[Command]
      commands.foreach { command =>
        val unit = stage.unit(command.unitId)
        if(
          !commandedIds.contains(unit.id) &&
          unit.owner.playerId == playerId &&
          unit.hp > 0
        ) {
          commandedIds += unit.id
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
          val workers = stage.field.unitsAt(resource.pos, owner = player, kind = CVUnitKind.Worker)
          player.resources += Math.min(workers.size, 5)
        }
      }
    }
  }
  object FinishingPhase {
    def result(stage: Stage): TurnResult = {
      val dead = (stage.castle1.hp <= 0, stage.castle2.hp <= 0)
      dead match {
        case (true, false) => TurnResult.P2Win
        case (false, true) => TurnResult.P1Win
        case (true, true) => TurnResult.Draw
        case (false, false) =>
          if(stage.turn >= 1000)
            TurnResult.Draw
          else
            TurnResult.InProgress
      }
    }
  }
}

case class CVUnit(id: Int, kind: CVUnitKind, owner: PlayerState, var pos: Pos) {
  def maxHp = kind.maxHp
  def attackRange = kind.attackRange
  def visibility = kind.visibility

  def movable(): Boolean = kind.movable

  var hp: Int = maxHp

  def hpString = s"${hp}/${maxHp}"

  def isVisible(pos: Pos): Boolean =
    this.pos.dist(pos) <= visibility

  def toView(): CVUnitView = CVUnitView(
    id = id,
    kind =kind,
    hp = hp,
    pos = pos
  )

  override def toString() =
    s"CVUnit(id=${id}, owner=${owner.playerId}, pos=${pos}, kind=${kind}, HP=${hp}/${maxHp})"
}

trait Thinker {
  def think(state: VisibleState): Seq[Command]
}

class Runner {
  def run(stage: Stage, player1: Thinker, player2: Thinker)(implicit logger: Logger): TurnResult = {
    var turnResult = run1(stage, player1, player2)
    while(turnResult == TurnResult.InProgress) {
      turnResult = run1(stage, player1, player2)
    }
    logger.stageEnd(stage, turnResult)
    turnResult
  }
  def run1(stage: Stage, player1: Thinker, player2: Thinker)(implicit logger: Logger): TurnResult = {
    val commands1 = player1.think(stage.visibleStateFor(1))
    val commands2 = player2.think(stage.visibleStateFor(2))

    stage.executeTurn(commands1, commands2)
  }
}

// Usage sample
object Main {
  def main(args: Array[String]): Unit = {
    val runner = new Runner
    implicit val logger = Logger.ShowAll
    implicit val random = new RandomSource
    runner.run(Stage.initialState(0),
      new Thinker {
        override def think(state: VisibleState): Seq[Command] = Seq()
      },
      new Thinker {
        override def think(state: VisibleState): Seq[Command] = Seq()
      }
    )
  }
}
