import com.wrapper.spotify.models.{SimplePlaylist, User}
import scala.annotation.tailrec

case class AppState(
                     user: Option[User] = None,
                     playlists: List[SimplePlaylist] = Nil,
                     tracksPerPlaylist: Map[Int, List[TrackWithFeatures]] = Map.empty
                   )

case class Operation(description: String, f: AppState => AppState)

object MyApp extends App {

  val numOptions = 5

  def options(optionNumber: Int): (AppState) => Map[Int, Operation] = (appState: AppState) => {
    val pre: Map[Int, Operation] = optionNumber match {
      case 0 => Map.empty
      case _ => options(optionNumber - 1)(appState)
    }
    pre ++ ((optionNumber, appState) match {
      case (1, _) => Map(1 -> Operation("Choose user", Actions.chooseUser))
      case (2, AppState(Some(_), _, _)) => Map(2 -> Operation("Get playlists", Actions.getPlaylists))
      case (3, AppState(Some(_), _, _)) => Map(3 -> Operation("Fetch everything", Actions.fetchAllPlaylistsWithTracks))
      case (4, AppState(Some(_), l, t)) if l.length == t.size && l.nonEmpty => Map(4 -> Operation("Show track info", Actions.getTrackInfo))
      case (5, AppState(Some(_), l, t)) if l.length == t.size && l.nonEmpty => Map(5 -> Operation("Show playlist averages", Actions.getPlaylistAverages))
      case _ => Map.empty
    })
  }

  def getOptions = options(numOptions)

  def runMenu(appState: AppState): Option[AppState] = {
    val opts = getOptions(appState)
    println("Choose option:")
    opts foreach { case (opt, Operation(desc, _)) => println(s"$opt. $desc") }
    println("0. Quit")
    Actions.readIntIn(opts.keySet + 0) match {
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
