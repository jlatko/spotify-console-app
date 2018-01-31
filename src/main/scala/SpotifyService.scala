import com.wrapper.spotify.Api
import com.wrapper.spotify.models.SimplePlaylist
import config.Config.{CLIENT_ID, CLIENT_SECRET}
import models.AudioFeatures
import org.json4s.native.JsonMethods.parse

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}

case class TrackWithFeatures(id: String, name: String, features: Option[AudioFeatures])

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

  def getPlaylistTracks(userId: String, playlistId: String): List[TrackWithFeatures] = {
    Try(mainClient.getPlaylistTracks(userId,playlistId).fields("items(track(id, name))").build().getJson) match {
      case Failure(_) =>
        println("Not found or request failed")
        Nil
      case Success(json) =>
        parse(json).\("items").children.flatMap( t => (t.\("track").values match {
          case track: Map[String,String] => track.get("id") -> track.get("name")
          case _ => None -> None
        }) match {
          case (Some(id), Some(name)) => Some(TrackWithFeatures(id,name,None))
          case _ => None
        })
    }
  }

  def getFeaturesForTracks(tracks: List[(String, String)]): List[TrackWithFeatures] = {
    Try(secondaryClient.AudioFeatures.getAudioFeatures(tracks.map{ t=>t._1 }.toSeq).get) match {
      case Success(features: Seq[AudioFeatures]) => tracks.zip(features).map{ case ( (id: String, name: String), feat: AudioFeatures) => TrackWithFeatures(id,name,Some(feat)) }
      case Failure(_) =>
        println("failed")
        tracks.map{ case (id,name) => TrackWithFeatures(id, name, None)}
    }
  }

  def getPlaylistTracksWithFeatures(userId: String, playlistId: String): List[TrackWithFeatures] = {
    val tracks = Try(mainClient.getPlaylistTracks(userId,playlistId).fields("items(track(id, name))").build().getJson) match {
      case Failure(_) =>
        println("Not found or request failed")
        Nil
      case Success(json) =>
        parse(json).\("items").children.flatMap( t => (t.\("track").values match {
          case track: Map[String,String] => track.get("id") -> track.get("name")
          case _ => None -> None
        }) match {
          case (Some(id), Some(name)) => Some(id->name)
          case _ => None
        })
    }

    getFeaturesForTracks(tracks.toList)
  }
}
