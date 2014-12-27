package com.todesking.codevs4.runner

import Ext._

import scala.collection.mutable.ArrayBuffer

sealed abstract class Direction {
  val flip: Direction
}
object Direction {
  case object Up    extends Direction { lazy val flip = Down }
  case object Down  extends Direction { lazy val flip = Up }
  case object Left  extends Direction { lazy val flip = Right }
  case object Right extends Direction { lazy val flip = Left }
}

class CVRandom {
  val random = new scala.util.Random

  // from(inclusive), to(inclusive)
  def nextInt(from: Int, to: Int): Int =
    from + random.nextInt(to - from + 1)
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
  def ranged(radius: Int): RangedPos =
    RangedPos(this, radius)
  override def toString() =
    s"($x, $y)"
}

case class RangedPos(center: Pos, radius: Int) {
  def contains(pos: Pos): Boolean =
    center.dist(pos) <= radius
}

abstract class CoordinateSystem {
  def toLocal(pos: Pos): Pos
  def toGlobal(pos: Pos): Pos
  def toLocal(dir: Direction): Direction
  def toGlobal(dir: Direction): Direction
}

object CoordinateSystem {
  val TopLeft = new CoordinateSystem {
    override def toLocal(pos: Pos) = pos
    override def toGlobal(pos: Pos) = pos
    override def toLocal(dir: Direction) = dir
    override def toGlobal(dir: Direction) = dir
  }
  val BottomRight = new CoordinateSystem {
    override def toLocal(pos: Pos) = Pos(99 - pos.x, 99 - pos.y)
    override def toGlobal(pos: Pos) = Pos(99 - pos.x, 99 - pos.y)
    override def toLocal(dir: Direction) = dir.flip
    override def toGlobal(dir: Direction) = dir.flip
  }
}

case class VisibleState(
  stageId: Int,
  turn: Int,
  resources: Int,
  playerUnits: Seq[CVUnitView],
  opponentUnits: Seq[CVUnitView],
  resourceLocations: Seq[Pos]
)

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

  def executeTurn(p1Command: Seq[Command], p2Command: Seq[Command]): TurnResult = {
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

sealed abstract class Command(val unitId: Int) {
}
object Command {
  case class Produce(override val unitId: Int, kind: CVUnit.Kind) extends Command(unitId)
  case class Move(override val unitId: Int, direction: Direction) extends Command(unitId)
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
    units.filter { unit => pos.ranged(dist).contains(unit.pos) }
  def unitsWithin(pos: Pos, dist: Int, owner: PlayerState): Seq[CVUnit] =
    unitsWithin(pos, dist).filter(_.owner == owner)

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

case class CVUnit(id: Int, kind: CVUnit.Kind, owner: PlayerState, var pos: Pos) {
  def maxHp = kind.maxHp
  def attackRange = kind.attackRange
  def visibility = kind.visibility

  def movable(): Boolean = kind.movable

  var hp: Int = maxHp

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

case class CVUnitView(
  id: Int,
  kind: CVUnit.Kind,
  hp: Int,
  pos: Pos
) {
  def maxHp = kind.maxHp
  def visibility = kind.visibility
  def movable(): Boolean = kind.movable
  def attackRange = kind.attackRange
  def isVisible(pos: Pos): Boolean =
    this.pos.dist(pos) <= visibility
}

object CVUnit {
  sealed abstract class Kind(
    val name: String,
    val code: String,
    val cost: Int,
    val attackRange: Int,
    val maxHp: Int,
    val visibility: Int,
    val movable: Boolean,
    val creatables: Set[Kind] = Set.empty
  ) {
    def canCreate(kind: Kind): Boolean =
      creatables.contains(kind)
    override def toString() =
      s"${name}"
  }
  object Kind {
    object Castle extends Kind(
      name = "Castle",
      code = "-",
      maxHp = 50000,
      attackRange = 10,
      visibility = 10,
      cost = 0,
      movable = false,
      creatables = Set(Worker)
    )
    object Worker extends Kind(
      name = "Worker",
      code = "0",
      maxHp = 2000,
      attackRange = 2,
      visibility = 10,
      cost = 40,
      movable = true,
      creatables = Set(Village)
    )
    object Knight extends Kind(
      name = "Knight",
      code = "1",
      maxHp = 5000,
      attackRange = 2,
      visibility = 4,
      cost = 20,
      movable = true
    )
    object Fighter extends Kind(
      name = "Fighter",
      code = "2",
      maxHp = 5000,
      attackRange = 2,
      visibility = 4,
      cost = 40,
      movable = true
    )
    object Assassin extends Kind(
      name = "Assassin",
      code = "3",
      maxHp = 5000,
      attackRange = 2,
      visibility = 4,
      cost = 60,
      movable = true
    )
    object Village extends Kind(
      name = "Village",
      code = "5",
      maxHp = 20000,
      attackRange = 2,
      visibility = 10,
      cost = 100,
      movable = false,
      creatables = Set(Worker)
    )
    object Barrack extends Kind(
      name = "Barrack",
      code = "6",
      maxHp = 20000,
      attackRange = 2,
      visibility = 4,
      cost = 500,
      movable = false,
      creatables = Set(Worker)
    )
  }
}

object DamageTable {
  def apply(attacker: CVUnit.Kind, defender: CVUnit.Kind): Int = {
    import CVUnit.Kind._
    (attacker, defender) match {
      case (Worker, _)                 => 100
      case (Knight, Worker)            => 100
      case (Knight, Knight)            => 500
      case (Knight, _)                 => 200
      case (Fighter, Worker)           => 500
      case (Fighter, Knight)           => 1600
      case (Fighter, _)                => 200
      case (Assassin, Worker|Fighter)  => 1000
      case (Assassin, Knight|Assassin) => 500
      case (Assassin, _)               => 200
      case (Castle, _)                 => 100
      case (Village, _)                => 100
      case (Barrack, _)                => 100
    }
  }
}

object Phase {
  object CommandPhase {
    def execute(stage: Stage, p1Command: Seq[Command], p2Command: Seq[Command]): Unit = {
      (sanitize(stage, p1Command, stage.player1.playerId) ++ sanitize(stage, p2Command, stage.player2.playerId)).foreach {
        case Command.Produce(unitId, kind) =>
          val unit = stage.unit(unitId)
          if(unit.kind.canCreate(kind) && unit.owner.hasEnoughResource(kind.cost)) {
            stage.createUnit(kind, unit.owner, unit.pos)
            unit.owner.consumeResource(kind.cost)
          }
        case Command.Move(unitId, direction) =>
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
          val workers = stage.field.unitsAt(resource.pos, owner = player, kind = CVUnit.Kind.Worker)
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
