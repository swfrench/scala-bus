
import NextBus.Feed

import spray.json._
import Feed.FeedJsonProtocol._

object Main {

  def main(args: Array[String]) {

    /* val agency = "actransit"
    val route = "18"
    val stopContains = "59th" */

    val agency = args(0)
    val route = args(1)
    val stopContains = args(2)

    val stops = Feed.getStops(agency, route)
      .filter { case (_,s) => s.title contains stopContains }

    if (stops.isEmpty)
      println("Cannot find stop containing \"" ++ stopContains ++ "\"")
    else
      stops map { case (tag,s) => {
          println("tag is " ++ tag ++ " for stop \"" ++ s.title ++ "\" - now running predictions query ...")
          val (predictions, messages) = Feed.Predictions(agency, route, tag).get
          println(predictions.mkString("\n"))
          //println(predictions.toJson.prettyPrint)
          println("messages for this query: " ++ messages.mkString("\n"))
        }
      }
  }
}
