import com.wrapper.spotify.models.{SimplePlaylist, User}
import models.AudioFeatures

import scala.annotation.tailrec
import scala.collection.JavaConversions._

case class AppState(
                     user: Option[User] = None,
                     playlists: List[SimplePlaylist] = Nil,
                     tracks: Map[String, (String, Option[AudioFeatures])] = Map.empty
                   )

case class Operation(description: String, f: AppState => AppState)

object MyApp extends App {

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
      println(s"Found: ${getAnyName(user)}")
      appState.copy(user = Some(user))
    } catch {
      case _: Throwable =>
        println("Not found or request failed")
        appState
    }
  }

  def getPlaylists: (AppState) => AppState = (appState: AppState) => {
    val playlists: Option[List[SimplePlaylist]] = try
      Some(SpotifyService.mainClient.getPlaylistsForUser(appState.user.get.getId).build().get().getItems.toList)
    catch {
      case _: Throwable => println("Not found or request failed"); None
    }
    playlists match {
      case None =>
        appState
      case Some(Nil) =>
        println("No playlists found")
        appState
      case Some(playlistsUnpacked) =>
        println(s"Playlists for user ${appState.user.get} ")
        playlistsUnpacked foreach (playlist => println(playlist.getName))
        appState.copy(playlists = playlistsUnpacked)
    }
  }

  val numOptions = 2

  def options(optionNumber: Int): (AppState) => Map[Int, Operation] = (appState: AppState) => {
    val pre: Map[Int, Operation] = optionNumber match {
      case 0 => Map.empty
      case _ => options(optionNumber - 1)(appState)
    }
    pre ++ ((optionNumber, appState) match {
      case (1, _) => Map(1 -> Operation("Choose user", chooseUser))
      case (2, AppState(Some(_), _, _)) => Map(2 -> Operation("Get playlists", getPlaylists))
      case _ => Map.empty
    })
  }

  def getOptions = options(numOptions)

  @tailrec
  def runMenu(appState: AppState): Option[AppState] = {
    val opts = getOptions(appState)
    println("Choose option:")
    opts foreach { case (opt, Operation(desc, _)) => println(s"$opt. $desc") }
    println("0. Quit")
    val x = try
      Some(scala.io.StdIn.readInt)
    catch {
      case _: Throwable => None
    }
    x match {
      case Some(0) => None
      case Some(key) if opts.keySet.contains(key) => Some(opts(key).f(appState))
      case _ =>
        println("wrong input")
        runMenu(appState)
    }
  }

  @tailrec
  def mainLoop(appState: AppState): Unit = {
    runMenu(appState) match {
      case Some(appState: AppState) => mainLoop(appState)
      case None => Unit
    }
  }

  mainLoop(AppState())

}
