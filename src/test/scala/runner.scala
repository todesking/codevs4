package com.todesking.codevs4.runner

import org.scalatest.{FunSpec, Matchers}
import org.scalatest.Inside._

class RunnerSpec extends RSpecLikeSpec with Matchers {
  implicit val rand = new CVRandom
  describe("ステージ(Stage)") {
    describeSubject("初期状態", Stage.initialState(99)) { subject =>
      it("ステージIDは指定されたものである") { subject().id shouldEqual 99 }
      it("ターン数 == 0") { subject().turn shouldEqual 0 }
      it("次に生成されるユニットのID == 12(初期ユニットぶん確保済み)") { subject().nextUnitID shouldEqual 12 }
      it("両陣営の資源数 == 0") {
        subject().player1.resources shouldEqual 0
        subject().player2.resources shouldEqual 0
      }
      it("各陣営は城1, ワーカー5が与えられる") {
        subject().players.foreach { player =>
          player.units.filter(_.kind == CVUnit.Kind.Castle).size shouldEqual 1
          player.units.filter(_.kind == CVUnit.Kind.Worker).size shouldEqual 5
        }
      }
      it("各陣営のユニットのHPは最大") {
        subject().players.foreach { player =>
          player.units.foreach { unit =>
            unit.hp shouldEqual unit.maxHp
          }
        }
      }
      it("プレイヤー1の城は(0, 0)の40マス圏内に配置される") {
        challange(100) { _ =>
          subject().player1.castle.pos.dist(Pos(0, 0)) should be <= 40
        }
      }
      it("プレイヤー2の城は(99, 99)の40マス圏内に配置される") {
        challange(100) { _ =>
          subject().player2.castle.pos.dist(Pos(99, 99)) should be <= 40
        }
      }
      it("初期ワーカーは各陣営の城と同じ場所に配置される") {
        challange(10) { _ =>
          subject().players.foreach { player =>
            player.units.filter(_.kind == CVUnit.Kind.Worker).foreach { worker =>
              worker.pos shouldEqual player.castle.pos
            }
          }
        }
      }
      describeSubject("フィールド(Field)", subject().field) { subject =>
        it("100x100マスで構成される") {
          subject().width shouldEqual 100
          subject().height shouldEqual 100
        }
        it("資源マスが(0, 0)の99マス圏内に10, (99, 99)の99マス圏内に10配置される") {
          challange(100) { _ =>
            val byDist = subject().resources.groupBy { r =>
              val topLeft = r.pos.dist(Pos(0, 0)) <= 98
              val bottomRight = r.pos.dist(Pos(99, 99)) <= 98
              (topLeft, bottomRight) match {
                case (true, false) => 0
                case (false, true) => 1
                case (false, false) => 2 // On border
                case (true, true) => assert(false)
              }
            }
            byDist.size should be >= 1
            byDist.size should be <= 3
            byDist.get(0).foreach(_.size should be <= 10)
            byDist.get(1).foreach(_.size should be <= 10)
            byDist.get(2).foreach(_.size should be <= 10)
            byDist.values.map(_.size).sum shouldEqual 20
          }
        }
        it("資源マス同士は重ならない") {
          challange(100) { _ =>
            subject().resources.groupBy(_.pos).size shouldEqual subject().resources.size
          }
        }
        it("資源マスは城の視野外に配置される") {
          challange(100) { _ =>
            subject().resources.foreach { resource =>
              subject().castle1.isVisible(resource.pos) shouldEqual false
              subject().castle2.isVisible(resource.pos) shouldEqual false
            }
          }
        }
      }
    }
    describeSubject("ターン進行", Stage.initialState(0)) { subject =>
      it("両者の入力を受け取ってターンを進め、結果を返す") {
        subject().player1.resources += 1000
        subject().field.addResource(Pos(50, 50))
        val w1 = subject().createUnit(CVUnit.Kind.Worker, subject().player1, Pos(50, 50))
        val w2 = subject().createUnit(CVUnit.Kind.Worker, subject().player2, Pos(51, 51))
        val p1Command = Seq(
          Command.Produce(w1.id, CVUnit.Kind.Village)
        )
        val p2Command = Seq(
          Command.Move(w2.id, Direction.Right)
        )
        subject().turn shouldEqual 0

        subject().executeTurn(p1Command, p2Command) shouldEqual TurnResult.InProgress

        subject().turn shouldEqual 1
        subject().field.unitsAt(Pos(50, 50)).size shouldEqual 2
        w1.hp shouldEqual(2000 - 50)
        w2.hp shouldEqual(2000 - 100 - 100)
        w1.pos shouldEqual Pos(50, 50)
        w2.pos shouldEqual Pos(50, 51)
        subject().player1.resources shouldEqual(1000 - 100 + 11)
        subject().player2.resources shouldEqual 10
      }
    }
    describe("結果生成") {
      val stage = let { Stage.minimalState() }
      before {
        stage().field.addResource(Pos(50, 50))
        stage().field.addResource(Pos(90, 90))
      }
      val w1 = let { stage().createUnit(CVUnit.Kind.Worker, stage().player1, Pos(51, 51)) }
      val w20 = let { stage().createUnit(CVUnit.Kind.Worker, stage().player2, Pos(51, 61)) }
      val w21 = let { stage().createUnit(CVUnit.Kind.Worker, stage().player2, Pos(62, 51)) }
      it("プレイヤー1に対するデータを生成できる") {
        inside(stage().visibleStateFor(1)) {
          case VisibleState(stageId, turn, resources, playerUnits, opponentUnits, resourceLocations) =>
            stageId           shouldEqual stage().id
            turn              shouldEqual stage().turn
            resources         shouldEqual stage().player1.resources
            playerUnits       shouldEqual stage().player1.units
            opponentUnits     shouldEqual Seq(w20())
            resourceLocations shouldEqual Seq(Pos(50, 50))
        }
      }
      it("プレイヤー2に対するデータを生成できる。座標系が適切に変換されていること。") {
        inside(stage().visibleStateFor(2)) {
          case VisibleState(stageId, turn, resources, playerUnits, opponentUnits, resourceLocations) =>
            stageId           shouldEqual stage().id
            turn              shouldEqual stage().turn
            resources         shouldEqual stage().player2.resources
            playerUnits       shouldEqual Seq(
              stage().castle2.copy(pos = Pos(0, 0)),
              w20().copy(pos = Pos(48, 38)),
              w21().copy(pos = Pos(37, 48)))
            opponentUnits     shouldEqual Seq(w1().copy(pos = Pos(48, 48)))
            resourceLocations shouldEqual Seq()
        }
      }
    }
    describe("アウトプット") {
      it("プレイヤーからのコマンドを解釈できる")(pending)
    }
    describe("シリアライズ") {
      it("現在の状態を文字列化し、そこから状態を復元することができる")(pending)
    }
  }
  describe("フェーズ進行") {
    describe("移動&生産フェーズ(Phase.Command)") {
      val stage = let(Stage.minimalState())

      describe("移動コマンド") {
        val worker1 = let { stage().createUnit(CVUnit.Kind.Worker, stage().player1, Pos(0, 0)) }
        val worker2 = let { stage().createUnit(CVUnit.Kind.Worker, stage().player1, Pos(99, 99)) }
        val worker3 = let { stage().createUnit(CVUnit.Kind.Worker, stage().player2, Pos(99, 99)) }
        val village = let { stage().createUnit(CVUnit.Kind.Village, stage().player1, Pos(50, 50)) }

        before {
          Phase.CommandPhase.execute(
            stage(),
            Seq(
              Command.Move(worker1().id, Direction.Down),
              Command.Move(worker2().id, Direction.Right),
              Command.Move(village().id, Direction.Up)
            ),
            Seq(
              Command.Move(worker3().id, Direction.Down)
            )
          )
        }

        describe("移動可能") {
          it("ユニットが指示された方向に移動する") {
            worker1().pos shouldEqual Pos(0, 1)
          }
          it("方向の指定はプレイヤーのローカル座標系で行われる") {
            worker3().pos shouldEqual Pos(99, 98)
          }
        }
        describe("移動不能(座標がフィールド外)") {
          it("何もしない") {
            worker2().pos shouldEqual Pos(99, 99)
          }
        }
        describe("移動不能(動けないユニット)") {
          it("何もしない") {
            village().pos shouldEqual Pos(50, 50)
          }
        }
      }

      val initialResource = 1000
      val initialNextUnitId = let(stage().nextUnitID)
      val initialUnitSize = let(stage().units.size)

      describe("生産コマンド") {
        describe("生産可能") {
          before {
            stage().player1.addResource(initialResource)
            Phase.CommandPhase.execute(
              stage(),
              Seq(Command.Produce(stage().castle1.id, CVUnit.Kind.Worker)),
              Seq()
            )
          }

          val createdUnits = let(stage().field.unitsAt(stage().castle1.pos).filter(_ != stage().castle1))

          it("コマンドを実行したユニットと同じ位置に新しいユニットが1体生成される") {
            val units = stage().field.unitsAt(stage().castle1.pos).filter(_ != stage().castle1)
            stage().units.size shouldEqual initialUnitSize() + 1
            createdUnits().size shouldEqual 1
            createdUnits().head.kind shouldEqual CVUnit.Kind.Worker
            createdUnits().head.pos shouldEqual stage().castle1.pos
          }
          it("生産したユニットのぶん資源が減る") {
            stage().player1.resources shouldEqual(initialResource - CVUnit.Kind.Worker.cost)
          }
          it("生産されたユニットのHPは最大") {
            createdUnits().head.hp shouldEqual createdUnits().head.maxHp
          }
          it("生産されたユニットのIDはStageに設定された次ID") {
            createdUnits().head.id shouldEqual initialNextUnitId()
          }
          it("Stageの次IDは+1される") {
            stage().nextUnitID shouldEqual initialNextUnitId() + 1
          }
        }
        describe("生産不能(不能な種別)") {
          before {
            stage().player1.addResource(initialResource)
            Phase.CommandPhase.execute(
              stage(),
              Seq(Command.Produce(stage().castle1.id, CVUnit.Kind.Castle)),
              Seq()
            )
          }
          it("ユニットは生産されない") {
            stage().units.size shouldEqual initialUnitSize()
          }
        }
        describe("生産不能(資源不足)") {
          before {
            stage().player1.addResource(0)
            Phase.CommandPhase.execute(
              stage(),
              Seq(Command.Produce(stage().castle1.id, CVUnit.Kind.Worker)),
              Seq()
            )
          }
          it("ユニットは生産されない") {
            stage().units.size shouldEqual initialUnitSize()
          }
        }
      }
      describe("何もしない") {
        before {
          Phase.CommandPhase.execute(
            stage(),
            Seq(),
            Seq()
          )
        }
        it("何もしない") {
          stage().player1.resources shouldEqual 0
          stage().units.size shouldEqual initialUnitSize()
        }
      }
      describe("1ユニットに対して複数のコマンド") {
        before {
          stage().player1.addResource(initialResource)
          Phase.CommandPhase.execute(
            stage(),
            Seq(
              Command.Produce(stage().castle1.id, CVUnit.Kind.Worker),
              Command.Produce(stage().castle1.id, CVUnit.Kind.Worker),
              Command.Produce(stage().castle1.id, CVUnit.Kind.Worker)
            ),
            Seq()
          )
        }
        it("最初のコマンド以外無視される") {
          stage().units.size shouldEqual initialUnitSize() + 1
        }
      }
      describe("指揮下にないユニットに対するコマンド") {
        before {
          stage().player1.addResource(initialResource)
          stage().player2.addResource(initialResource)
          Phase.CommandPhase.execute(
            stage(),
            Seq(Command.Produce(stage().castle2.id, CVUnit.Kind.Worker)),
            Seq()
          )
        }
        it("無視される") {
          stage().units.size shouldEqual initialUnitSize()
        }
      }
      describe("死んだユニットに対するコマンド") {
        before {
          stage().player1.addResource(initialResource)
          stage().castle1.hp = 0
          Phase.CommandPhase.execute(
            stage(),
            Seq(Command.Produce(stage().castle1.id, CVUnit.Kind.Worker)),
            Seq()
          )
        }
        it("無視される") {
          stage().units.size shouldEqual initialUnitSize()
        }
      }
    }
    describe("戦闘フェーズ(Phase.Battle)") {
      val stage = let(Stage.minimalState())
      describe("ユニットの攻撃範囲に敵がいない場合") {
        val unit1 = let { stage().createUnit(CVUnit.Kind.Worker, stage().player1, Pos(50, 50)) }
        val unit2 = let { stage().createUnit(CVUnit.Kind.Worker, stage().player2, Pos(50, 50 + 20)) }

        before { Phase.BattlePhase.execute(stage()) }

        it("何もしない") {
          unit1().hp shouldEqual unit1().maxHp
          unit2().hp shouldEqual unit2().maxHp
        }
      }
      describe("攻撃範囲に敵がいるユニットの場合") {
        describe("敵1体") {
          val worker = let { stage().createUnit(CVUnit.Kind.Worker, stage().player1, Pos(50, 50)) }
          val knight = let { stage().createUnit(CVUnit.Kind.Knight, stage().player2, Pos(50, 50 + 1)) }
          before { Phase.BattlePhase.execute(stage()) }

          it("攻撃力ぶんのダメージを与える") {
            worker().hp shouldEqual(worker().maxHp - DamageTable(knight().kind, worker().kind))
          }
        }
        describe("1マスに複数(<=10)の敵がスタックされている場合") {
          val worker = let { stage().createUnit(CVUnit.Kind.Worker, stage().player1, Pos(50, 50)) }
          val knights = let {
            (1 to 10).map { _ =>
              stage().createUnit(CVUnit.Kind.Knight, stage().player2, Pos(50, 50 + 1))
            } ++ Seq(
              stage().createUnit(CVUnit.Kind.Knight, stage().player2, Pos(50 + 1, 50))
            )
          }

          before { Phase.BattlePhase.execute(stage()) }

          it("範囲内のすべての敵に、攻撃力を敵の合計で割ったダメージ(切り捨て)を与える") {
            knights().foreach { knight =>
              knight.hp shouldEqual(knight.maxHp - DamageTable(worker().kind, knight.kind) / knights().size)
            }
          }
        }
        describe("1マスに複数(>10)の敵がスタックされている場合") {
          val worker = let { stage().createUnit(CVUnit.Kind.Worker, stage().player1, Pos(50, 50)) }
          val knights = let {
            (1 to 100).map { _ =>
              stage().createUnit(CVUnit.Kind.Knight, stage().player2, Pos(50, 50 + 1))
            } ++ Seq(
              stage().createUnit(CVUnit.Kind.Knight, stage().player2, Pos(50 + 1, 50))
            )
          }

          before { Phase.BattlePhase.execute(stage()) }
          it("範囲内のすべての敵に、攻撃力を敵の合計で割ったダメージ(切り捨て)を与える(10>のスタックは10と計算)") {
            val k = 10 + 1
            knights().foreach { knight =>
              knight.hp shouldEqual(knight.maxHp - DamageTable(worker().kind, knight.kind) / k)
            }
          }
        }
      }
    }
    describe("ユニット除外フェーズ") {
      val stage = let(Stage.minimalState())
      before {
        stage().units.size shouldEqual 2
        val u = stage().createUnit(CVUnit.Kind.Worker, stage().player1, Pos(0, 0))
        stage().units.size shouldEqual 3

        u.hp = 0
        Phase.SweepPhase.execute(stage())
      }
      it("フィールド上に存在するHP<0のユニットは、取り除かれる") {
        stage().units.size shouldEqual 2
        stage().units.filter(_.kind == CVUnit.Kind.Castle).size shouldEqual 2
      }
    }
    describe("資源獲得フェーズ") {
      val stage = let { Stage.minimalState() }
      val resourcePos = Pos(50, 50)
      before {
        stage().field.addResource(resourcePos)
      }
      describe("ワーカーが資源上にいないとき") {
        before {
          stage().createUnit(CVUnit.Kind.Worker, stage().player1, Pos(0, 0))
          stage().player1.resources shouldEqual 0
          Phase.ResourcingPhase.execute(stage())
        }
        it("基本収入として+10される") {
          stage().player1.resources shouldEqual 10
        }
      }
      describe("ワーカーが資源上にいる(<=5)") {
        before {
          stage().createUnit(CVUnit.Kind.Worker, stage().player1, resourcePos)
          (1 to 5).foreach { _ =>
            stage().createUnit(CVUnit.Kind.Worker, stage().player2, resourcePos)
          }
          Phase.ResourcingPhase.execute(stage())
        }
        it("資源上にいるワーカーの数に応じて資源が増える") {
          stage().player1.resources shouldEqual 11
          stage().player2.resources shouldEqual 15
        }
      }
      describe("同一資源上のワーカーの数>5") {
        before {
          (1 to 6).foreach { _ =>
            stage().createUnit(CVUnit.Kind.Worker, stage().player1, resourcePos)
          }
          (1 to 10).foreach { _ =>
            stage().createUnit(CVUnit.Kind.Worker, stage().player2, resourcePos)
          }
          Phase.ResourcingPhase.execute(stage())
        }
        it("ワーカーの数が5とみなされる") {
          stage().player1.resources shouldEqual 15
          stage().player2.resources shouldEqual 15
        }
      }
    }
    describe("終了フェーズ") {
      val stage = let { Stage.minimalState() }
      describe("1000ターン以内、両者の城HP>=0") {
        val result = let { Phase.FinishingPhase.result(stage()) }
        it("試合中と判定される") {
          result() shouldEqual TurnResult.InProgress
        }
      }
      describe("P1の城HPのみ<=0") {
        before {
          stage().castle1.hp = 0
        }
        val result = let { Phase.FinishingPhase.result(stage()) }
        it("P2の勝ちと判定される") {
          result() shouldEqual TurnResult.P2Win
        }
      }
      describe("P2の城HPのみ<=0") {
        before {
          stage().castle2.hp = 0
        }
        val result = let { Phase.FinishingPhase.result(stage()) }
        it("P1の勝ちと判定される") {
          result() shouldEqual TurnResult.P1Win
        }
      }
      describe("両者の城HP<=0") {
        before {
          stage().castle1.hp = 0
          stage().castle2.hp = 0
        }
        val result = let { Phase.FinishingPhase.result(stage()) }
        it("引き分けと判定される") {
          result() shouldEqual TurnResult.Draw
        }
      }
      describe("両者の城HP>0かつ1000ターン経過した") {
        before {
          stage().turn = 1000
        }
        val result = let { Phase.FinishingPhase.result(stage()) }
        it("引き分けと判定される") {
          result() shouldEqual TurnResult.Draw
        }
      }
    }
  }
}
