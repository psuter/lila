package lila.tournament
package swiss

import lila.tournament.{ Score => AbstractScore }

import scala.util.Try

object SwissSystem extends PairingSystem with ScoringSystem {
  private type STour = swisssystem.Tournament[String]

  case class Score(win: Option[Boolean], value: Int) extends AbstractScore

  case class Sheet(scores: List[Score]) extends ScoreSheet {
    def total: Int = scores.map { _.win match {
      case None        => 1
      case Some(true)  => 2
      case Some(false) => 0 
    }} sum
  }

  def scoreSheet(player: String, tour: Tournament): Sheet = {
    Sheet(Nil)
  }

  def createNewPairings(users: List[String], pairings: Pairings, nbActiveUsers: Int): Pairings = {
    Nil
  }

  private def constructFromHistory(tour: Tournament): STour = {
    val history = tour.pairingsAndEvents

    swisssystem.Tournament.create(Map.empty, 7, 2)
  }
}

