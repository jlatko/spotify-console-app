import com.wrapper.spotify.methods.Request
import com.wrapper.spotify.models.{SimplePlaylist, User}
import models.AudioFeatures
//import org.json4s._
//import org.json4s.native.JsonMethods.parse

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}

case class AppState(
                     user: Option[User] = None,
                     playlists: List[SimplePlaylist] = Nil,
                     tracksPerPlaylist: Map[Int, List[TrackWithFeatures]] = Map.empty
                   )

case class Operation(description: String, f: AppState => AppState)
object MyApp extends App {

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
        println("wrong input")
        readIntIn(available)
    }
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
      if (user != appState.user.orNull){
        println(s"Found: ${getAnyName(user)}")
        appState.copy(user = Some(user), playlists = Nil, tracksPerPlaylist = Map.empty)}
      else{
        println("Already fetched")
        appState}
    } catch {
      case _: Throwable =>
        println("Not found or request failed")
        appState
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

  def getPlaylistTracks: (AppState) => AppState = (appState: AppState) => {
    println("Choose list")
    appState.playlists.zipWithIndex foreach{ case (playlist, index) => println(s"$index. ${playlist.getName}")}
    val i = readIntIn(appState.playlists.indices.toSet)
    val tracks = SpotifyService.getPlaylistTracks(appState.user.get.getId, appState.playlists(i).getId)
    tracks foreach (t => println(t.name))
    appState.copy(tracksPerPlaylist = appState.tracksPerPlaylist + (i ->  tracks))
  }

  def fetchAllPlaylistsWithTracks: (AppState) => AppState = (appState: AppState) => {
    val newState = getPlaylists(appState)
    val indices = newState.playlists.indices.toList
    newState.copy(tracksPerPlaylist = indices.map{ i =>
      (i, SpotifyService.getPlaylistTracksWithFeatures(newState.user.get.getId, newState.playlists(i).getId))
    }.toMap)
  }

  val numOptions = 4
  def options(optionNumber: Int): (AppState) => Map[Int, Operation] = (appState: AppState) => {
    val pre: Map[Int, Operation] = optionNumber match {
      case 0 => Map.empty
      case _ => options(optionNumber - 1)(appState)
    }
    pre ++ ((optionNumber, appState) match {
      case (1, _) => Map(1 -> Operation("Choose user", chooseUser))
      case (2, AppState(Some(_), _, _)) => Map(2 -> Operation("Get playlists", getPlaylists))
      case (3, AppState(Some(_), _, _)) => Map(3 -> Operation("Fetch everything", fetchAllPlaylistsWithTracks))
      case (4, AppState(Some(_), _::_, _)) => Map(4 -> Operation("Get tracks from playlist", getPlaylistTracks))
      case _ => Map.empty
    })
  }

  def getOptions = options(numOptions)

  def runMenu(appState: AppState): Option[AppState] = {
    val opts = getOptions(appState)
    println("Choose option:")
    opts foreach { case (opt, Operation(desc, _)) => println(s"$opt. $desc") }
    println("0. Quit")
    readIntIn(opts.keySet + 0) match {
      case 0 => None
      case key if opts.keySet.contains(key) => Some(opts(key).f(appState))
    }
  }

  @tailrec
  def mainLoop(appState: AppState): Unit = {
    runMenu(appState) match {
      case Some(newState: AppState) => mainLoop(newState)
      case None => Unit
    }
  }

  mainLoop(AppState())

}
