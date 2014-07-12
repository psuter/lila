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
    val pairingSystem = swiss.SwissSystem
    val scoringSystem = swiss.SwissSystem
  }

  val default = Rush

  val all = List(Rush, Swiss)

  val byId = all map { s => (s.id -> s) } toMap

  def apply(id: Int): Option[System] = byId get id
  def orDefault(id: Int): System = apply(id) getOrElse default
}

trait PairingSystem {
  def createPairings(tournament: Tournament, users: List[String]): (Pairings,Events)
}

trait Score {
  val value: Int
}

trait ScoreSheet {
  def scores: List[Score]
  def total:  Int
}

trait ScoringSystem {
  type Sheet <: ScoreSheet

  // You must override either this one of the other !
  def scoreSheets(tournament: Tournament): Map[String,Sheet] = {
    tournament.players.map { p =>
      (p.id -> scoreSheet(tournament, p.id))
    } toMap
  } 

  // You must override either this one of the other !
  def scoreSheet(tournament: Tournament, player: String): Sheet = scoreSheets(tournament)(player)
}
