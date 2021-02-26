package controllers

import akka.actor.{ Actor, ActorRef, ActorSystem, Props }
import akka.stream.Materializer
import com.mohiva.play.silhouette.api.Silhouette
import play.api.mvc.{ AbstractController, Action, AnyContent, ControllerComponents, Request, WebSocket }
import utils.auth.DefaultEnv
import de.htwg.se.stratego.Stratego
import de.htwg.se.stratego.controller.controllerComponent.{ ControllerInterface, FieldChanged, GameFinished, PlayerSwitch }
import de.htwg.se.stratego.model.matchFieldComponent.matchFieldBaseImpl.{ Field, Matrix }

import javax.inject.{ Inject, Singleton }
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.impl.providers.GoogleTotpInfo
import play.api.libs.json.{ JsNumber, JsObject, JsValue, Json, Writes }
import play.api.libs.streams.ActorFlow

import scala.concurrent.{ ExecutionContext, Future }
import scala.swing.Reactor

@Singleton
class StrategoController @Inject() (
  scc: SilhouetteControllerComponents,
  about: views.html.stratego,
  menu: views.html.strategoMenu,
  matchfield: views.html.strategoMatchfield,
  silhouette: Silhouette[DefaultEnv]
)(implicit ex: ExecutionContext, system: ActorSystem, mat: Materializer) extends SilhouetteController(scc) {

  val gameController: ControllerInterface = Stratego.controller
  var playerName1 = ""
  var playerName2 = ""
  var clientPlayerIndex = 0
  var playerCounter = 0
  var gameFinished = false

  def aboutpage = SecuredAction.async { implicit request: SecuredRequest[EnvType, AnyContent] =>
    authInfoRepository.find[GoogleTotpInfo](request.identity.loginInfo).map { totpInfoOpt =>
      Ok(about(request.identity, totpInfoOpt))
    }
  }

  def menupage = SecuredAction.async { implicit request: SecuredRequest[EnvType, AnyContent] =>
    authInfoRepository.find[GoogleTotpInfo](request.identity.loginInfo).map { totpInfoOpt =>
      Ok(menu(request.identity, totpInfoOpt))
    }
  }

  def matchfieldpage = SecuredAction.async { implicit request: SecuredRequest[EnvType, AnyContent] =>
    authInfoRepository.find[GoogleTotpInfo](request.identity.loginInfo).map { totpInfoOpt =>
      Ok(matchfield(request.identity, totpInfoOpt))
    }
  }

  def load = SecuredAction.async { implicit request: SecuredRequest[EnvType, AnyContent] =>
    authInfoRepository.find[GoogleTotpInfo](request.identity.loginInfo).map { totpInfoOpt =>
      Stratego.controller.load
      Ok(matchfield(request.identity, totpInfoOpt))
    }
  }

  def save = SecuredAction.async { implicit request: SecuredRequest[EnvType, AnyContent] =>
    authInfoRepository.find[GoogleTotpInfo](request.identity.loginInfo).map { totpInfoOpt =>
      Stratego.controller.save
      Ok(matchfield(request.identity, totpInfoOpt))
    }
  }

  def undo = SecuredAction.async { implicit request: SecuredRequest[EnvType, AnyContent] =>
    authInfoRepository.find[GoogleTotpInfo](request.identity.loginInfo).map { totpInfoOpt =>
      Stratego.controller.undo
      Ok(matchfield(request.identity, totpInfoOpt))
    }
  }

  def redo = SecuredAction.async { implicit request: SecuredRequest[EnvType, AnyContent] =>
    authInfoRepository.find[GoogleTotpInfo](request.identity.loginInfo).map { totpInfoOpt =>
      Stratego.controller.redo
      Ok(matchfield(request.identity, totpInfoOpt))
    }
  }

  def setPlayers(player1: String, player2: String): Action[AnyContent] = Action {
    gameController.createEmptyMatchfield(gameController.getSize)
    gameController.setPlayers(player1 + " " + player2)
    gameController.initMatchfield
    Redirect(controllers.routes.StrategoController.matchfieldpage())
  }

  def setPlayer(): Action[JsValue] = Action(parse.json) {
    setPlayerRequest: Request[JsValue] =>
      {
        val playerName = (setPlayerRequest.body \ "playerName").as[String]

        if (playerCounter == 2) {
          playerCounter = 0
          clientPlayerIndex = 0
        }

        if (playerCounter == 0) {
          playerName1 = playerName
          playerCounter = 1
        } else {
          gameFinished = false
          playerName2 = playerName
          gameController.createEmptyMatchfield(gameController.getSize)
          gameController.setPlayers(playerName1 + " " + playerName2)
          clientPlayerIndex = 1
          playerCounter = 2
          gameController.initMatchfield
        }

        Ok(Json.obj(
          "clientPlayerIndex" -> JsNumber(clientPlayerIndex)
        ))
      }
  }

  def move: Action[JsValue] = Action(parse.json) {
    moveRequest: Request[JsValue] =>
      {
        val dir = (moveRequest.body \ "dir").as[String].toCharArray
        val row = (moveRequest.body \ "row").as[Int]
        val col = (moveRequest.body \ "col").as[Int]

        if (dir(0) == 'r') {
          val rowD = row + 1
          if (rowD >= 0 && rowD < gameController.getField.matrixSize) {
            if (gameController.getField.field(col, rowD).isSet) {
              gameController.attack(col, row, col, rowD)
            }
          }
          gameController.move(dir(0), col, row)

        } else if (dir(0) == 'l') {
          val rowD = row - 1
          if (rowD >= 0 && rowD < gameController.getField.matrixSize) {
            if (gameController.getField.field(col, rowD).isSet) {
              gameController.attack(col, row, col, rowD)
            }
          }
          gameController.move(dir(0), col, row)

        } else if (dir(0) == 'd') {
          val colD = col + 1
          if (colD >= 0 && colD < gameController.getField.matrixSize) {
            if (gameController.getField.field(colD, row).isSet) {
              gameController.attack(col, row, colD, row)
            }
          }
          gameController.move(dir(0), col, row)

        } else if (dir(0) == 'u') {
          val colD = col - 1
          if (colD >= 0 && colD < gameController.getField.matrixSize) {
            if (gameController.getField.field(colD, row).isSet) {
              gameController.attack(col, row, colD, row)
            }
          }
          gameController.move(dir(0), col, row)
        }

        Ok(Json.obj(
          "matchField" -> Json.toJson(
            for {
              row <- 0 until gameController.getField.matrixSize
              col <- 0 until gameController.getField.matrixSize
            } yield {
              var obj = Json.obj(
                "row" -> row,
                "col" -> col
              )
              if (gameController.getField.field(row, col).isSet) {
                obj = obj.++(Json.obj(
                  "figName" -> gameController.getField.field(row, col).character.get.figure.name,
                  "figValue" -> gameController.getField.field(row, col).character.get.figure.value,
                  "colour" -> gameController.getField.field(row, col).colour.get.value
                )
                )
              }
              obj
            }
          ),
          "currentPlayerIndex" -> JsNumber(gameController.currentPlayerIndex),
          "currentPlayer" -> (gameController.playerList(gameController.currentPlayerIndex)).toString()
        ))
      }
  }

  def gameToJson: Action[AnyContent] = Action {
    Ok(Json.obj(
      "currentPlayerIndex" -> JsNumber(gameController.currentPlayerIndex),
      "currentPlayer" -> (gameController.playerList(gameController.currentPlayerIndex)).toString(),
      "players" -> (gameController.playerList.head + " " + gameController.playerList(1)),
      "matchField" -> Json.toJson(
        for {
          row <- 0 until gameController.getField.matrixSize
          col <- 0 until gameController.getField.matrixSize
        } yield {
          var obj = Json.obj(
            "row" -> row,
            "col" -> col
          )
          if (gameController.getField.field(row, col).isSet) {
            obj = obj.++(Json.obj(
              "figName" -> gameController.getField.field(row, col).character.get.figure.name,
              "figValue" -> gameController.getField.field(row, col).character.get.figure.value,
              "colour" -> gameController.getField.field(row, col).colour.get.value
            )
            )
          }
          obj
        }
      )
    ))
  }

  def socket: WebSocket = WebSocket.accept[String, String] { request =>
    ActorFlow.actorRef { out =>
      println("Connect received")
      StrategoWebSocketActorFactory.create(out)
    }
  }

  object StrategoWebSocketActorFactory {
    def create(out: ActorRef): Props = {
      Props(new StrategoWebSocketActor(out))
    }
  }

  class StrategoWebSocketActor(out: ActorRef) extends Actor with Reactor {
    listenTo(gameController)

    override def receive: Receive = {
      case msg: String =>
        out ! Json.obj(
          "currentPlayerIndex" -> JsNumber(gameController.currentPlayerIndex),
          "currentPlayer" -> (gameController.playerList(gameController.currentPlayerIndex)).toString(),
          "players" -> (gameController.playerList.head + " " + gameController.playerList(1)),
          "matchField" -> Json.toJson(
            for {
              row <- 0 until gameController.getField.matrixSize
              col <- 0 until gameController.getField.matrixSize
            } yield {
              var obj = Json.obj(
                "row" -> row,
                "col" -> col
              )
              if (gameController.getField.field(row, col).isSet) {
                obj = obj.++(Json.obj(
                  "figName" -> gameController.getField.field(row, col).character.get.figure.name,
                  "figValue" -> gameController.getField.field(row, col).character.get.figure.value,
                  "colour" -> gameController.getField.field(row, col).colour.get.value
                )
                )
              }
              obj
            }
          )
        ).toString()
    }

    reactions += {
      case event: FieldChanged => sendJsonToClient()
      case event: PlayerSwitch => sendJsonToClient()
      case event: GameFinished => {
        gameFinished = true
        sendJsonToClient()
      }
    }

    def sendJsonToClient(): Unit = {
      println("Received event from Controller")
      out ! Json.obj(
        "currentPlayerIndex" -> JsNumber(gameController.currentPlayerIndex),
        "currentPlayer" -> (gameController.playerList(gameController.currentPlayerIndex)).toString(),
        "players" -> (gameController.playerList.head + " " + gameController.playerList(1)),
        "gameFinished" -> (gameFinished.toString),
        "matchField" -> Json.toJson(
          for {
            row <- 0 until gameController.getField.matrixSize
            col <- 0 until gameController.getField.matrixSize
          } yield {
            var obj = Json.obj(
              "row" -> row,
              "col" -> col
            )
            if (gameController.getField.field(row, col).isSet) {
              obj = obj.++(Json.obj(
                "figName" -> gameController.getField.field(row, col).character.get.figure.name,
                "figValue" -> gameController.getField.field(row, col).character.get.figure.value,
                "colour" -> gameController.getField.field(row, col).colour.get.value
              )
              )
            }
            obj
          }
        )
      ).toString()
    }
  }
}
