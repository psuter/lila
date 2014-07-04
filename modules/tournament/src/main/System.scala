package lila.tournament

sealed abstract class System(val id: Int)

object System {
  case object Rush extends System(id = 1)
  case object Swiss extends System(id = 2)

  val default = Rush

  val all = List(Rush, Swiss)

  val byId = all map { s => (s.id -> s) } toMap

  def apply(id: Int): Option[System] = byId get id
  def orDefault(id: Int): System = apply(id) getOrElse default
}
