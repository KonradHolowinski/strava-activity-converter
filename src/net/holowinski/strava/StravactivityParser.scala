package net.holowinski.strava

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.time.Instant

import scala.io.Source
import stravactivity._
import tcx._

import scala.annotation.tailrec

object StravactivityParser {

  def main(args: Array[String]) {
    val tcx = parse(args(0))
    Files.write(Paths.get(args(1)), tcx.toXml.getBytes(StandardCharsets.UTF_8))
  }

  def parse(path: String) = {
    val tokens = tokenizeLines(Source.fromFile(path).getLines())
    val extWaypoints = splitByWaypointsWithAdditionalFields(tokens)
    val trackpoints = extWaypoints map extendedWaypointToTcxTrackpoint

    Tcx(
      List(Activity(
        sport = "Biking",
        id = trackpoints.head.time.toString,
        laps = List(Lap(
          startTime = trackpoints.head.time,
          totalTimeSeconds = Some(extWaypoints.last.waypoint.time - extWaypoints.head.waypoint.time toInt),
          track = Track(trackpoints),
          distanceMeters = Some(extWaypoints.last.waypoint.distance),
          maximumSpeed = Some(extWaypoints.map(_.waypoint.speed).max)
        ))
      ))
    )
  }


  def extendedWaypointToTcxTrackpoint(waypoint: WaypointWithExtraFields): Trackpoint = {
    val wp = waypoint.waypoint
    Trackpoint(
      time = Instant.ofEpochSecond(wp.time.toLong),
      position = Position(latitude = wp.latitude, longitude = wp.longitude),
      speed = Some(wp.speed),
      watts = waypoint.power,
      cadence = waypoint.cadence.map(_.toInt),
      distance = Some(wp.distance),
      altitude = Some(wp.altitude)
    )
  }

  def splitByWaypointsWithAdditionalFields(tokens: List[Token]): List[WaypointWithExtraFields] = {

    @tailrec
    def split(tokens: List[Token], agg: List[WaypointWithExtraFields]): List[WaypointWithExtraFields] = {
      val wp = takeWaypointWithAdditionalFields(tokens)
      if (wp._1.isEmpty) agg
      else split(wp._2, agg ::: List(wp._1.get))
    }
    split(tokens, Nil)
  }

  @tailrec
  def takeWaypointWithAdditionalFields(tokens: List[Token]): (Option[WaypointWithExtraFields], List[Token]) =
    if (tokens.isEmpty) (None, tokens)
    else tokens.head match {
      case t: Waypoint => {
        val span = tokens.tail.span {
          case t: Waypoint => false
          case _ => true
        }
        (buildWaypointWithExtraFields(t, span._1), span._2)
      }
      case _ => takeWaypointWithAdditionalFields(tokens.tail)
    }

  def buildWaypointWithExtraFields(waypoint: Waypoint, extraTokens: List[Token]): Some[WaypointWithExtraFields] = {
    Some(WaypointWithExtraFields(
      waypoint,
      extraTokens.flatMap {
        case x: Cadence => Some(x.value)
        case _ => None
      }.headOption,
      extraTokens.flatMap {
        case x: Power => Some(x.value)
        case _ => None
      }.headOption
    ))
  }

  def tokenizeLines(lines: Iterator[String]): List[Token] = {
    lines.map(Token.fromLine).toList.flatten
  }

}

case class WaypointWithExtraFields(waypoint: Waypoint, cadence: Option[Double], power: Option[Double])
