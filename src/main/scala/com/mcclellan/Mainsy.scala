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
import java.io.FileInputStream
import java.io.BufferedInputStream
import spray.httpx.marshalling.BasicMarshallers

object MetadataFileDirectory {
	private val Pattern = "(.*)\\.metadata$".r
	def findReturnName(metaFilename : String, folder : File) : String = metaFilename match {
		case Pattern(c) => {
			val returnFiles = folder.listFiles.filter(file => file.getName.startsWith(c) && !file.getName.endsWith(".metadata")).map(_.getPath)
			assert(returnFiles.size == 1, "Invalid number of files to return, found " + returnFiles + ". Ensure each metadata file only has one associated file")
			returnFiles.head
		}
		case _ => throw new RuntimeException("Failed to find file for metadata file " + metaFilename)
	}
}

class MetadataFileDirectory(val path : File) {
	lazy val metadataFiles = path.listFiles().filter(_.getName().endsWith(".metadata")).map(file => {
		(MetadataFileDirectory.findReturnName(file.getName(), path),
			Source.fromFile(file).getLines.map(line => line.split("=", 2).toList match {
				case key :: value :: Nil => (key.trim, value.trim)
				case key :: Nil => throw new RuntimeException("Missing value for key " + key)
				case key :: value :: rs => throw new RuntimeException("Parse of metadata failed, had too many values found for " +
					"key %s, value %s, others %s".format(key, value, rs))
				case Nil => throw new RuntimeException("Parse of metadata failed, had an empty list after splitting on '='")
			}).toMap)
	}).toMap
}

class Service extends Actor with HttpService {
	lazy val metadataMaps = new MetadataFileDirectory(new File("c:\\Development\\metadata")).metadataFiles
	val bigIntMarshaller = Marshaller.of[BigInt](ContentType.`text/plain`) { (value, contentType, ctx) =>
		ctx.marshalTo(HttpBody(contentType, value.toString + " "))
	}
	
	def actorRefFactory = context

	def receive = handleTimeouts orElse runRoute(
		get {
			path("ping") { ctx =>
				val params = ctx.request.queryParams.toSet
				metadataMaps.find(entry => {
					val intersection = (entry._2.toSet intersect params)
					intersection.size == entry._2.size && intersection.size == params.size 
				}) match {
					case Some((key, map)) => {
						val bis = new BufferedInputStream(new FileInputStream(key))
						ctx.complete(StatusCodes.OK, HttpHeaders.`Content-Disposition`("inline", Map("filename" -> key)) :: Nil, Stream.continually(bis.read).takeWhile(_ != -1).map(_.toByte).toArray)
					}
					case None => {
						println("No mapping for " + params)
						ctx.complete(StatusCodes.NotFound, Nil, "")
					}
				}
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