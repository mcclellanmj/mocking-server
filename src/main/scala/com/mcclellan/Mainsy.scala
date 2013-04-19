package com.mcclellan

import spray.can.server.SprayCanHttpServerApp
import akka.actor.Actor
import akka.actor.Props
import spray.http.HttpRequest
import spray.routing.HttpService
import spray.http.Timeout
import spray.http.StatusCodes
import spray.http.HttpResponse
import spray.http.HttpHeaders
import spray.http.MediaTypes
import spray.http.MediaRanges
import spray.http.HttpBody

class Service extends Actor with HttpService {
	def actorRefFactory = context

	def receive = handleTimeouts orElse runRoute(
		get {
			path("ping") { ctx =>
				println(ctx.request.queryParams)
				ctx.complete(
					HttpResponse(
						status = StatusCodes.Conflict,
						headers = HttpHeaders.Accept(MediaRanges.`application/*`) :: Nil,
						entity = HttpBody(MediaTypes.`application/json`, "pong")))
			}
		})

	def handleTimeouts : Receive = {
		case Timeout(x : HttpRequest) =>
			sender ! HttpResponse(StatusCodes.InternalServerError, "Request timed out.")
	}
}

object Mainsy extends App with SprayCanHttpServerApp {
	val me = system.actorOf(Props[Service])
	newHttpServer(me) ! Bind(interface = "localhost", port = 8080)
}