package com.todesking.codevs4.runner

import org.scalatest.{FunSpec, Matchers}

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
          player.units.filter(_.isInstanceOf[Castle]).size shouldEqual 1
          player.units.filter(_.isInstanceOf[Worker]).size shouldEqual 5
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
        subject.challange(100) { _ =>
          subject().player1.castle.pos.dist(Pos(0, 0)) should be <= 40
        }
      }
      it("プレイヤー2の城は(99, 99)の40マス圏内に配置される") {
        subject.challange(100) { _ =>
          subject().player2.castle.pos.dist(Pos(99, 99)) should be <= 40
        }
      }
      it("初期ワーカーは各陣営の城と同じ場所に配置される") {
        subject.challange(10) { _ =>
          subject().players.foreach { player =>
            player.units.filter(_.isInstanceOf[Worker]).foreach { worker =>
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
          subject.challange(100) { _ =>
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
          subject.challange(100) { _ =>
            subject().resources.groupBy(_.pos).size shouldEqual subject().resources.size
          }
        }
        it("資源マスは城の視野外に配置される") {
          subject.challange(100) { _ =>
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
        val p1Command = Seq(Command.Nop)
        val p2Command = Seq(Command.Nop)
        subject().turn shouldEqual 0
        subject().step(p1Command, p2Command) shouldEqual StepResult.InProgress
        subject().turn shouldEqual 1
      }
    }
    describe("インプット") {
      it("現在の状態から、各プレイヤーに対するインプットデータを生成できる")(pending)
    }
    describe("アウトプット") {
      it("プレイヤーからのコマンドを解釈できる")(pending)
    }
    describe("シリアライズ") {
      it("現在の状態を文字列化し、そこから状態を復元することができる")(pending)
    }
  }
  describe("フェーズ進行") {
    describeSubject("移動&生産フェーズ(Phase.Command)", Stage.minimalState()) { stage =>
      describe("移動コマンド") {
        describe("移動可能") {
          it("ユニットが指示された方向に移動する")(pending)
        }
        describe("移動不能()") {
          it("何もしない")(pending)
        }
      }

      val initialResource = 100
      describe("生産コマンド") {
        describeSubject("生産可能", {
          stage().player1.addResource(initialResource)
          Phase.CommandPhase.execute(
            stage(),
            Seq(Command.Production(stage().castle1, CVUnit.Kind.Worker)),
            Seq()
          )
          stage()
        }) { stage =>
          it("コマンドを実行したユニットと同じ位置に新しいユニットが生成される") {
            val units = stage().field.unitsAt(stage().castle1.pos).filter(_ != stage().castle1)
            units.size shouldEqual 1
            units.head.kind shouldEqual CVUnit.Kind.Worker
          }
          it("生産したユニットのぶん資源が減る") {
            stage().player1.resources shouldEqual(initialResource - CVUnit.Kind.Worker.cost)
          }
          it("生産されたユニットのHPは最大")(pending)
          it("生産されたユニットのIDはStageに設定された次ID")(pending)
          it("Stageの次IDは+1される")(pending)
        }
        describe("生産不能(不能な種別)") {
          it("何もしない")(pending)
        }
        describe("生産不能(資源不足)") {
          it("何もしない")(pending)
        }
      }
      describe("何もしない") {
        it("何もしない")(pending)
      }
      describe("1ユニットに対して複数のコマンド") {
        it("最初のコマンド以外無視される")(pending)
      }
      describe("指揮下にないユニットに対するコマンド") {
        it("無視される")(pending)
      }
    }
    describe("戦闘フェーズ") {
      describe("攻撃範囲に敵がいないユニットの場合") {
        it("何もしない")(pending)
      }
      describe("攻撃範囲に敵がいるユニットの場合") {
        describe("敵1体") {
          it("攻撃力ぶんのダメージを与える")(pending)
        }
        describe("1マスに複数(<=10)の敵がスタックされている場合") {
          it("範囲内のすべての敵に、攻撃力を敵の合計で割ったダメージ(切り捨て)を与える")(pending)
        }
        describe("1マスに複数(>10)の敵がスタックされている場合") {
          it("範囲内のすべての敵に、攻撃力を敵の合計で割ったダメージ(切り捨て)を与える(10>のスタックは10と計算)")(pending)
        }
      }
    }
    describe("ユニット除外フェーズ") {
      it("フィールド上に存在するHP<0のユニットは、取り除かれる")(pending)
    }
    describe("資源獲得フェーズ") {
      it("基本収入として+10される")(pending)
      it("資源上にいるワーカーの数に応じて資源が増える")(pending)
      it("同一の資源上にいる自軍ワーカーの数>5だったら5とみなされる")(pending)
    }
    describe("終了フェーズ") {
      describe("1000ターン以内、両者の城HP>=0") {
        it("試合中と判定される")(pending)
      }
      describe("P1の城HPのみ<=0") {
        it("P2の勝ちと判定される")(pending)
      }
      describe("P1の城HPのみ<=0") {
        it("P1の勝ちと判定される")(pending)
      }
      describe("両者の城HP<=0") {
        it("引き分けと判定される")(pending)
      }
      describe("両者の城HP>0かつ1000ターン経過した") {
        it("引き分けと判定される")(pending)
      }
    }
  }
  describe("ユニット(Unit)") {
    describe("城(Castle)") {
      it("ワーカーを生産できる")(pending)
    }
    describe("村(Village)") {
      it("ワーカーを生産できる")(pending)
    }
    describe("拠点(Base)") {
      it("戦闘ユニットを生産できる")(pending)
    }
  }
}
