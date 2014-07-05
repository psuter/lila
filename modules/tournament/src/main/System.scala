package lila.tournament

sealed abstract class System(val id: Int) {
  val pairingSystem: PairingSystem
}

object System {
  case object Rush extends System(id = 1) {
    val pairingSystem = rush.PairingSystem
  }

  case object Swiss extends System(id = 2) {
    val pairingSystem = rush.PairingSystem
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
