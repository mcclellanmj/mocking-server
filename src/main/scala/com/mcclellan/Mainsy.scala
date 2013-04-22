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
import java.io.File
import scala.io.Source
import scala.collection.mutable.MapBuilder

class FileAttributes {
	private val builder = new MapBuilder[String, String, Map[String, String]](Map())
	def addLine(line : String) = {
		val List(key : String, value : String, rs) = line.split("=", 1).toList
		assert(rs.isEmpty(), "Error, splitting line yielded more than one value. " +
				"key = %s, value = %s, remaining = %s".format(key, value, rs))
		builder += ((key, value))
	}
}

class Files {
	val metadataFiles = new File("~/MockImages/").listFiles().filter(_.getName().endsWith(".metadata"))
	
	metadataFiles.map(file => {
		val attributes = new FileAttributes
		Source.fromFile(file).getLines.foreach(line => {
			attributes.addLine(line)
		})
		attributes
	})
}

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