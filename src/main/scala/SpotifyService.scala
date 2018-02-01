import com.wrapper.spotify.Api
import com.wrapper.spotify.models.SimplePlaylist
import config.Config.{CLIENT_ID, CLIENT_SECRET}
import models.AudioFeatures
import org.json4s.native.JsonMethods.parse
import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}

case class TrackWithFeatures(id: String, name: String, artist: String, features: Option[AudioFeatures]) {
  def showInfo(): Unit = {
    println(name)
    features match {
      case Some(f) =>
        print(
      s"""
    acousticness: ${f.acousticness},
    danceability: ${f.danceability},
    duration_ms: ${f.duration_ms},
    energy: ${f.energy},
    instrumentalness: ${f.instrumentalness},
    key: ${f.key},
    liveness: ${f.liveness},
    loudness: ${f.loudness},
    mode: ${f.mode},
    speechiness: ${f.speechiness},
    tempo: ${f.tempo},
    time_signature: ${f.time_signature},
    valence: ${f.valence}
       """)
      case _ => Unit
    }
  }

  def getFeaturesAsArray(): Array[Double] = {
    features match {
      case Some(f: AudioFeatures) =>   Array(
        f.acousticness,
        f.danceability,
        f.duration_ms,
        f.energy,
        f.instrumentalness,
        f.liveness,
        f.loudness,
        f.mode,
        f.speechiness,
        f.tempo,
        f.time_signature,
        f.valence
      )
      case _ => null
    }
  }
}

object SpotifyService {
  val mainClient: Api = Api.builder().
    clientId(CLIENT_ID).
    clientSecret(CLIENT_SECRET).
    build
  val token: String = mainClient.clientCredentialsGrant.build.get.getAccessToken
  mainClient.setAccessToken(token)
  val secondaryClient: SpotifyClient = new SpotifyClient(token) // the main one does not support AudioFeatures

  def getPlaylists(userId: String): List[SimplePlaylist] = {
    Try(mainClient.getPlaylistsForUser(userId).build().get().getItems.toList) match {
      case Failure(_) =>
        println("Not found or request failed")
        Nil
      case Success(Nil) =>
        println("No playlists found")
        Nil
      case Success(playlistsUnpacked) =>
        playlistsUnpacked
    }
  }

  def printFeatures(f: Array[Double]): Unit = {
    if(f.length == 12){
      print(
        s"""
    acousticness: ${f(0)},
    danceability: ${f(1)},
    duration_ms: ${f(2)},
    energy: ${f(3)},
    instrumentalness: ${f(4)},
    liveness: ${f(5)},
    loudness: ${f(6)},
    mode: ${f(7)},
    speechiness: ${f(8)},
    tempo: ${f(9)},
    time_signature: ${f(10)},
    valence: ${f(11)}
       """)
    }
  }

  def getFeaturesForTracks(tracks: List[(String, String, String)]): List[TrackWithFeatures] = {
    Try(secondaryClient.AudioFeatures.getAudioFeatures(tracks.map{ t=>t._1 }.toSeq).get) match {
      case Success(features: Seq[AudioFeatures]) =>
        tracks.zip(features).map{
          case ( (id: String, name: String, artist: String), feat: AudioFeatures) => TrackWithFeatures(id,name, artist,Some(feat))
        }
      case Failure(_) =>
        println("failed")
        tracks.map{ case (id,name, artist) => TrackWithFeatures(id, name, artist, None)}
    }
  }

  def getPlaylistTracksWithFeatures(userId: String, playlistId: String): List[TrackWithFeatures] = {
    val tracks = Try(mainClient.getPlaylistTracks(userId,playlistId).fields("items(track(id, name, artists(name)))").build().getJson) match {
      case Failure(_) =>
        println("Not found or request failed")
        Nil
      case Success(json) =>
        parse(json).\("items").children.flatMap( t => (t.\("track").values match {
          case track: Map[String,Any] => (
            track.get("id"),
            track.get("name"),
            track.get("artists") match {
              case Some(a: List[Map[String, String]]) => a.flatMap((m) => m.get("name")) mkString ", "
              case _ => ""
            })
          case _ => ()
        }) match {
          case (Some(id: String), Some(name: String), artists: String) => Some(id, name, artists)
          case _ => None
        })
    }

    getFeaturesForTracks(tracks.toList)
  }
}
