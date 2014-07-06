package lila.tournament

sealed abstract class System(val id: Int) {
  val pairingSystem: PairingSystem
  val scoringSystem: ScoringSystem
}

object System {
  case object Rush extends System(id = 1) {
    val pairingSystem = rush.PairingSystem
    val scoringSystem = rush.ScoringSystem
  }

  case object Swiss extends System(id = 2) {
    val pairingSystem = rush.PairingSystem
    val scoringSystem = rush.ScoringSystem
  }

  val default = Rush

  val all = List(Rush, Swiss)

  val byId = all map { s => (s.id -> s) } toMap

  def apply(id: Int): Option[System] = byId get id
  def orDefault(id: Int): System = apply(id) getOrElse default
}

trait PairingSystem {
  def createNewPairings(users: List[String], pairings: Pairings, nbActiveUsers: Int): Pairings
}

trait Score {
  val win: Option[Boolean]
  val value: Int
}

trait ScoreSheet {
  def scores: List[Score]
  def total:  Int
}

trait ScoringSystem {
  type Sheet <: ScoreSheet

  def scoreSheet(player: String, tournament: Tournament): Sheet
}
