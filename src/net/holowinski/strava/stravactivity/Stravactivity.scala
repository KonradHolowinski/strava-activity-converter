package net.holowinski.strava.stravactivity

sealed trait Token

object Token {
  def fromLine(line: String): Option[Token] = line match {
    case l if l.startsWith("wp:") => Waypoint.fromLine(l)
    case l if l.startsWith("pow:") => Power.fromLine(l)
    case l if l.startsWith("cad:") => Cadence.fromLine(l)
    case _ => None
  }
}


case class Waypoint(
 latitude: Double,
 longitude: Double,
 horizontalAcceleration: Double,
 verticalAcceleration: Double,
 altitude: Double,
 speed: Double,
 course: Double,
 time: Double,
 distance: Double
) extends Token

object Waypoint {
  val regex = """wp: lat:(\d+\.\d+); long:(\d+\.\d+); hacc:(\d+\.\d+); vacc:(\d+\.\d+); alt:(\d+\.\d+); speed:(\d+\.\d+); course:(\d+\.\d+); t:(\d+\.\d+); dt:\d+\.\d+; dist:(\d+\.\d+)""".r

  def fromLine(line: String): Option[Waypoint] = {
    line match {
      case regex(latitude, longitude, hAcc, vAcc, altitude, speed, course, time, distance) =>
        Some(Waypoint(latitude.toDouble, longitude.toDouble, hAcc.toDouble, vAcc.toDouble, altitude.toDouble, speed.toDouble, course.toDouble, time.toDouble, distance.toDouble))
      case _ => None
    }
  }
}

case class Power(value: Double, time: Double) extends Token

object Power {
  val regex = """pow: v:(\d+\.\d+); t:(\d+\.\d+);""".r

  def fromLine(line: String): Option[Power] = {
    line match {
      case regex(value, time) => Some(Power(value.toDouble, time.toDouble))
      case _ => None
    }
  }
}

case class Cadence(value: Double, time: Double) extends Token

object Cadence {
  val regex = """cad: v:(\d+\.\d+); t:(\d+\.\d+);""".r

  def fromLine(line: String): Option[Cadence] = {
    line match {
      case regex(value, time) => Some(Cadence(value.toDouble, time.toDouble))
      case _ => None
    }
  }
}