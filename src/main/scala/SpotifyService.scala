import com.wrapper.spotify.Api
import config.Config.{CLIENT_ID, CLIENT_SECRET}


object SpotifyService {
  val mainClient: Api = Api.builder().
    clientId(CLIENT_ID).
    clientSecret(CLIENT_SECRET).
    build
  val token: String = mainClient.clientCredentialsGrant.build.get.getAccessToken
  mainClient.setAccessToken(token)
  val secondaryClient: SpotifyClient = new SpotifyClient(token) // the main one does not support AudioFeatures
}
