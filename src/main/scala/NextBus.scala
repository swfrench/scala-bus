
/** A simple interface for querying the Next Bus XML Feed REST API
  * 
  * Currently supports only a limited subset of the API, in particular the
  * "routeList", "routeConfig", and "predictions" commands.
  * 
  * Result elements of each are in general encapsulated in case classes for
  * easy pattern matching, which further provide `JsonFormat`s for cast to and
  * from JSON (via `spray.json`).
  */
package NextBus

import scala.collection.immutable.{Seq,Map}

import spray.json._

object Feed {

  /** Base URL for the Next Bus XML feed
    */
  val baseURL = "http://webservices.nextbus.com/service/publicXMLFeed"

  /** Abstract utility class for querying a REST interface that returns XML
    */
  abstract class Query(val url: String) {

    val params: Map[String,String]

    def apply(): scala.xml.Node = {
      val paramString = params
        .map({ case (k,v) => k + "=" + v })
        .mkString("&")
      def resp(ps: String): String =
        scala.io.Source.fromURL(url + "?" + ps)
          .mkString
      scala.xml.XML.loadString(resp(paramString))
    }

  }


  /** Query the XML feed for route information (`routeList`)
    */
  class RoutesQuery(agency: String) extends Query(baseURL) {
    val params = Map("command" -> "routeList", "a" -> agency)
  }

  /** Query the XML feed for stop information (`routeConfig`)
    */
  class StopsQuery(agency: String, route: String) extends Query(baseURL) {
    val params = Map("command" -> "routeConfig", "a" -> agency, "r" -> route)
  }

  /** Query the XML feed for bus predictions (`predictions`)
    */
  class PredictionsQuery(agency: String, route: String, stop: String) extends Query(baseURL) {
    val params = Map("command" -> "predictions", "a" -> agency, "r" -> route, "s" -> stop)
  }


  /** A single route info element
    */
  case class Route(tag: String, title: String, shortTitle: String) {
    override def toString: String =
      "Route " ++ tag ++ ": " ++ title ++ " (" ++ shortTitle ++ ")"
  }

  /** A single stop info element
    */
  case class Stop(tag: String, title: String, shortTitle: String, stopId: String) {
    override def toString: String =
      "Stop " ++ tag ++ ": " ++ title ++ " (" ++ shortTitle ++ ") [" ++ stopId ++ "]"
  }

  /** A single prediction info element
    */
  case class Prediction(minutes: String, dirTitle: String, dirTag: String) {
    override def toString: String =
      "Bus in " ++ minutes ++ " minutes (direction: " ++ dirTitle ++ ")"
  }

  /** `JsonFormat` support via `spray.json` for the `Route`, `Stop`, and 
    * `Prediction` classes
    */
  object FeedJsonProtocol extends DefaultJsonProtocol {
    implicit val routeFormat = jsonFormat3(Route)
    implicit val stopFormat = jsonFormat4(Stop)
    implicit val predictionFormat = jsonFormat3(Prediction)
  }


  /*
   * Parsers for XML responses
   */

  def parseRoutes(resp: scala.xml.Node): Seq[Route] =
    (resp \\ "body" \ "route") map ((r) => Route(
      (r \ "@tag").text,
      (r \ "@title").text,
      (r \ "@shortTitle").text))

  def parseStops(resp: scala.xml.Node): Seq[Stop] =
    (resp \\ "body" \ "route" \ "stop") map ((r) => Stop(
      (r \ "@tag").text,
      (r \ "@title").text,
      (r \ "@shortTitle").text,
      (r \ "@stopId").text))

  def parsePredictions(resp: scala.xml.Node): (Seq[Prediction],Seq[String]) = {

    val predictions: scala.xml.NodeSeq = resp \\ "body" \ "predictions"

    val predictionMessages: Seq[String] =
      (predictions \ "message") map ((m) => (m \ "@text").text)

    def parsePredictionDirection(d: scala.xml.Node): Seq[Prediction] =
      (d \ "prediction") map ((p) => Prediction(
        (p \ "@minutes").text,
        (d \ "@title").text,
        (p \ "@dirTag").text))

    ((predictions \ "direction") flatMap parsePredictionDirection, predictionMessages)
  }


  /** Fetch route information for the specified `agency`, returned as an
    * immutable `Map` keyed on the route tag (used in queries)
    */
  def getRoutes(agency: String): Map[String,Route] = {
    val q = new RoutesQuery(agency)
    parseRoutes(q())
      .map((r) => (r.tag,r))
      .toMap
  }

  /** Fetch stop informaton for the specified `agency` and `route`, returned
    * as an immutable `Map` keyed on the stop tag (used in queries)
    */
  def getStops(agency: String, route: String): Map[String,Stop] = {
    val q = new StopsQuery(agency, route)
    parseStops(q())
      .map((s) => (s.tag,s))
      .toMap
  }

  /** Poll (possibly repeatedly) the Feed for prediction info associated with
    * the supplied `agency`, `route`, and `stop` (via the `get` method)
    */
  class Predictions(val agency: String, val route: String, val stop: String) {
    val q = new PredictionsQuery(agency, route, stop)
    /** Returns tuple of predictions and associated messages (each as an
      * immutable `Seq`).
      */
    def get = parsePredictions(q())
  }

  object Predictions {
    def apply(agency: String, route: String, stop: String) =
      new Predictions(agency, route, stop)
  }

}
