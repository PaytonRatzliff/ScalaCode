import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.StatusCodes._
import akka.stream.scaladsl.Flow

import scala.concurrent.{ExecutionContext, Future}
/**
  * Created by pratzlif on 8/10/17.
  */
object AkkaRunner extends App{
  implicit val system: ActorSystem = ActorSystem("Dummy")
  implicit val executionContext: ExecutionContext = system.dispatcher

  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val httpSettings = ("0.0.0.0", 9998)

  def routes: HttpRequest => Future[HttpResponse] = {
    httpRequest: HttpRequest =>
      (httpRequest.method, httpRequest.uri.path.toString()) match {
        case (GET, "/flipCoin") => Marshal(TestCommands.flipCoin.toString).to[ResponseEntity] map { entity =>
          HttpResponse(
            status = OK,
            entity=entity
          )
        }case (POST, "/flipCoin") =>
          val numCoins = httpRequest.entity.toString.split("&text=")(1).split("&")(0)
          val number = if(numCoins != "") numCoins.toInt else 1
          Marshal("Helping out").to[ResponseEntity] map { entity =>
            val response = HttpResponse(
              status = OK
            ).withEntity(ContentTypes.`application/json`, s"""{"response_type": "in_channel","text": ${TestCommands.flipNCoins(number)}}""")
            println(response.entity)
            response
        }
        case _ =>
          val responseBody = s"Resource not found: ${httpRequest.method.value} ${httpRequest.uri.path}"
          Marshal(responseBody).to[ResponseEntity] map { entity =>
            HttpResponse(
              status = OK,
              entity=entity
            )
          }
      }
  }

  val serverBinding: Future[ServerBinding] =
    Http()
    .bindAndHandle(
      handler = Flow[HttpRequest].mapAsync(1)(routes),
      interface = "0.0.0.0",
      port = 9998
    )
}
