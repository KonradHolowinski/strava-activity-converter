package net.holowinski.strava.tcx

import java.time.Instant

case class Tcx(activities: List[Activity]) extends XmlEntry {
  override def toXml: String =
    s"""
       <?xml version="1.0" encoding="UTF-8"?>
       <TrainingCenterDatabase xsi:schemaLocation="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2 http://www.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd" xmlns:ns5="http://www.garmin.com/xmlschemas/ActivityGoals/v1" xmlns:ns3="http://www.garmin.com/xmlschemas/ActivityExtension/v2" xmlns:ns2="http://www.garmin.com/xmlschemas/UserProfile/v2" xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
         <Activities>
         ${activities.map(_.toXml).mkString}
         </Activities>
       </TrainingCenterDatabase>
  """
}

case class Activity(sport: String, id: String, laps: List[Lap]) extends XmlEntry {
  override def toXml: String =
    s"""
       <Activity Sport="$sport">
          <Id>$id</Id>
          ${laps.map(_.toXml).mkString}
       </Activity>
    """
}

case class Lap(
                startTime: Instant,
                totalTimeSeconds: Option[Int] = None,
                distanceMeters: Option[Double]= None,
                maximumSpeed: Option[Double]= None,
                calories: Option[Int]= None,
                averageHeartRate: Option[Int]= None,
                maximumHeartRate: Option[Int]= None,
                intensity: Option[String]= Some("Active"),
                cadence: Option[Int]= None,
                triggerMethod: Option[String] = Some("Manual"),
                track: Track
              ) extends XmlEntry {
  override def toXml: String =
    s"""<Lap StartTime="$startTime">
            ${El("TotalTimeSeconds", totalTimeSeconds).toXml}
            ${El("DistanceMeters", distanceMeters).toXml}
            ${El("MaximumSpeed", maximumSpeed).toXml}
            ${El("Calories", calories).toXml}
            ${ElWithValue("AverageHeartRateBpm", averageHeartRate).toXml}
            ${ElWithValue("MaximumHeartRateBpm", maximumHeartRate).toXml}
            ${El("Intensity", intensity).toXml}
            ${El("Cadence", cadence).toXml}
            ${El("TriggerMethod", triggerMethod).toXml}
            ${track.toXml}
        </Lap>"""
}

case class Track(trackpoints: List[Trackpoint]) extends XmlEntry {
  override def toXml: String =
    s"""<Track>
        ${trackpoints.map(_.toXml).mkString}
        </Track>
    """
}


case class Trackpoint(
  time: Instant,
  position: Position,
  altitude: Option[Double] = None,
  distance: Option[Double]= None,
  heartRate: Option[Int] = None,
  cadence: Option[Int] = None,
  speed: Option[Double] = None,
  watts: Option[Double] = None
) extends XmlEntry {

  def buildExtensions() =
    if (speed.isDefined || watts.isDefined) s"""
         <Extensions>
          <TPX xmlns="http://www.garmin.com/xmlschemas/ActivityExtension/v2">
           ${El("Speed", speed).toXml}
           ${El("Watts", watts).toXml}
          </TPX>
         </Extensions>
       """
    else ""

  override def toXml: String =
    s"""<Trackpoint>
              <Time>$time</Time>
              ${position.toXml}
              ${El("AltitudeMeters", altitude).toXml}
              ${El("DistanceMeters", distance).toXml}
              ${ElWithValue("HeartRateBpm", heartRate).toXml}
              ${El("Cadence", cadence).toXml}
              ${buildExtensions()}
        </Trackpoint>"""
}

case class Position(latitude: Double, longitude: Double) extends XmlEntry {
  override def toXml: String =
    s"""<Position>
               <LatitudeDegrees>$latitude</LatitudeDegrees>
               <LongitudeDegrees>$longitude</LongitudeDegrees>
              </Position>"""
}


case class ElWithValue[T](name: String, value: Option[T]) extends XmlEntry {
  override def toXml: String = value match {
    case Some(v) => s"<$name><Value>$value</Value></$name>"
    case None => ""
  }
}

case class El[T](name: String, value: Option[T]) extends XmlEntry {
  override def toXml: String = value match {
    case Some(v) => s"<$name>$v</$name>"
    case None => ""
  }
}

abstract class XmlEntry {
  def toXml: String
}
