package com.todesking.codevs4.runner

class PlayerState {
  var resources: Int = 0
}

class Stage(
  val id: Int,
  var turn: Int = 0,
  var nextUnitID: Int = 12
) {
  val player1 = new PlayerState()
  val player2 = new PlayerState()
  val field = new Field()
}

object Stage {
  def initialState(stageId: Int): Stage =
    new Stage(stageId)
}

class Field {
  val width: Int = 100
  val height: Int = 100
}
