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
import spray.routing.Directive
import spray.httpx.marshalling.MetaMarshallers
import spray.httpx.marshalling.Marshaller
import spray.http.ContentType
import spray.http.HttpEntity

class Service extends Actor with HttpService {
	lazy val fibs : Stream[BigInt] =0 #::
                                1 #::
                                fibs.zip(fibs.tail).map { n => n._1 + n._2 }
	
	val bigIntMarshaller = Marshaller.of[BigInt](ContentType.`text/plain`) { (value, contentType, ctx) =>
		ctx.marshalTo(HttpBody(contentType,value.toString + " " ))
	}
	
	def actorRefFactory = context

	def receive = handleTimeouts orElse runRoute(
		get {  
			path("ping") { ctx =>
				implicit val marshaller : Marshaller[Stream[BigInt]] = Marshaller.streamMarshaller(bigIntMarshaller, actorRefFactory)
				ctx.complete(StatusCodes.OK, Nil, fibs)
			}
		})

	def handleTimeouts : Receive = {
		case Timeout(x : HttpRequest) =>
			sender ! HttpResponse(StatusCodes.InternalServerError, "Request timed out.")
	}
}

object Mainsy extends App with SprayCanHttpServerApp {
	val me = system.actorOf(Props[Service])
	newHttpServer(me) ! Bind(interface = "localhost", port = 8888)
}