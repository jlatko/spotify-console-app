import java.util.NoSuchElementException

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import com.sun.org.apache.xerces.internal.impl.xpath.XPath.Axis
import com.wrapper.spotify.models.{SimplePlaylist, User}

import scala.annotation.tailrec


object Actions {

  @tailrec
  def readIntIn(available: Set[Int]): Int = {
    val x = try
      Some(scala.io.StdIn.readInt)
    catch {
      case _: Throwable => None
    }
    x match {
      case Some(key) if available.contains(key) => key
      case _ =>
        println("wrong input, try again")
        readIntIn(available)
    }
  }

  def getPlaylistAverages(appState: AppState): AppState = {
    val i = choosePlaylist(appState.playlists)
    try
      if (i != -1){
        val playlist = appState.playlists(i)
        val tracks = appState.tracksPerPlaylist(i)
        println(s"playlist: ${playlist.getName}, tracks: ${tracks.length}")
        val tracksMatrix = DenseMatrix.apply(tracks.map(_.getFeaturesAsArray()):_*)
        val avg = (sum(tracksMatrix(::, *)).inner/ tracks.length.toDouble).data
        SpotifyService.printFeatures(avg)
      }
      catch {
      case _ @ (_ : IndexOutOfBoundsException | _: NoSuchElementException) => println("Something went wrong. Try to fetch everything again")
    }
    appState
  }

  def getAnyName(user: User): String =
    user.getDisplayName match {
      case null | "" => user.getId
      case x => x
    }

  def chooseUser: (AppState) => AppState = (appState: AppState) => {
    println("Enter user id:")
    val id = scala.io.StdIn.readLine
    try {
      val user = SpotifyService.mainClient.getUser(id).build().get()
      if (user != appState.user.orNull) {
        println(s"Found: ${getAnyName(user)}")
        appState.copy(user = Some(user), playlists = Nil, tracksPerPlaylist = Map.empty)
      }
      else {
        println("Already fetched")
        appState
      }
    } catch {
      case _: Throwable =>
        println("Not found or request failed")
        appState
    }
  }

  def chooseFromList[A](showFunc: (A => String))(l: List[A]): Int = {
    println("Choose")
    l.zipWithIndex foreach { case (item, index) => println(s"${index + 1}. ${showFunc(item)}") }
    println("0. Back")
    readIntIn(l.indices.toSet + l.length) - 1
  }

  def choosePlaylist: List[SimplePlaylist] => Int = chooseFromList[SimplePlaylist]((playlist: SimplePlaylist) => playlist.getName)

  def chooseTrack: List[TrackWithFeatures] => Int = chooseFromList[TrackWithFeatures]((track: TrackWithFeatures) => track.name)

  @tailrec
  def chooseTrackFrom(appState: AppState, playlistInd: Option[Int]): Option[(Int, Int)] = {
    playlistInd match {
      case None => chooseTrackFrom(appState, Some(choosePlaylist(appState.playlists)))
      case Some(-1) => None
      case Some(i) =>
        chooseTrack(appState.tracksPerPlaylist(i)) match {
          case -1 => chooseTrackFrom(appState, None)
          case t => Some(i, t)
        }
    }
  }

  def getPlaylists: (AppState) => AppState = (appState: AppState) => {
    SpotifyService.getPlaylists(appState.user.get.getId) match {
      case Nil => appState
      case playlists =>
        println(s"Playlists for user ${getAnyName(appState.user.get)} ")
        playlists foreach (playlist => println(playlist.getName))
        appState.copy(playlists = playlists)
    }
  }

  def getTrackInfo: (AppState) => AppState = (appState: AppState) => {
    chooseTrackFrom(appState, None) match {
      case Some((playlistInd, trackInd)) => appState.tracksPerPlaylist(playlistInd)(trackInd).showInfo()
      case _ =>
    }
    appState
  }

  def fetchAllPlaylistsWithTracks: (AppState) => AppState = (appState: AppState) => {
    val newState = getPlaylists(appState)
    val indices = newState.playlists.indices.toList
    newState.copy(tracksPerPlaylist = indices.map { i =>
      (i, SpotifyService.getPlaylistTracksWithFeatures(newState.user.get.getId, newState.playlists(i).getId))
    }.toMap)
  }
}
