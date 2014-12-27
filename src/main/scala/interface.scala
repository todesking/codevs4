package com.todesking.codevs4.interface

sealed abstract class Direction {
  val flip: Direction
}
object Direction {
  case object Up    extends Direction { lazy val flip = Down }
  case object Down  extends Direction { lazy val flip = Up }
  case object Left  extends Direction { lazy val flip = Right }
  case object Right extends Direction { lazy val flip = Left }
}

class RandomSource {
  val random = new scala.util.Random

  // from(inclusive), to(inclusive)
  def nextInt(from: Int, to: Int): Int =
    from + random.nextInt(to - from + 1)
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

sealed abstract class Command(val unitId: Int) {
}
object Command {
  case class Produce(override val unitId: Int, kind: CVUnitKind) extends Command(unitId)
  case class Move(override val unitId: Int, direction: Direction) extends Command(unitId)
}

case class CVUnitView(
  id: Int,
  kind: CVUnitKind,
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

sealed abstract class CVUnitKind(
  val name: String,
  val code: String,
  val cost: Int,
  val attackRange: Int,
  val maxHp: Int,
  val visibility: Int,
  val movable: Boolean,
  val creatables: Set[CVUnitKind] = Set.empty
) {
  def canCreate(kind: CVUnitKind): Boolean =
    creatables.contains(kind)
  override def toString() =
    s"${name}"
}
object CVUnitKind {
  object Castle extends CVUnitKind(
    name = "Castle",
    code = "-",
    maxHp = 50000,
    attackRange = 10,
    visibility = 10,
    cost = 0,
    movable = false,
    creatables = Set(Worker)
  )
  object Worker extends CVUnitKind(
    name = "Worker",
    code = "0",
    maxHp = 2000,
    attackRange = 2,
    visibility = 10,
    cost = 40,
    movable = true,
    creatables = Set(Village)
  )
  object Knight extends CVUnitKind(
    name = "Knight",
    code = "1",
    maxHp = 5000,
    attackRange = 2,
    visibility = 4,
    cost = 20,
    movable = true
  )
  object Fighter extends CVUnitKind(
    name = "Fighter",
    code = "2",
    maxHp = 5000,
    attackRange = 2,
    visibility = 4,
    cost = 40,
    movable = true
  )
  object Assassin extends CVUnitKind(
    name = "Assassin",
    code = "3",
    maxHp = 5000,
    attackRange = 2,
    visibility = 4,
    cost = 60,
    movable = true
  )
  object Village extends CVUnitKind(
    name = "Village",
    code = "5",
    maxHp = 20000,
    attackRange = 2,
    visibility = 10,
    cost = 100,
    movable = false,
    creatables = Set(Worker)
  )
  object Barrack extends CVUnitKind(
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

object DamageTable {
  def apply(attacker: CVUnitKind, defender: CVUnitKind): Int = {
    import CVUnitKind._
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
