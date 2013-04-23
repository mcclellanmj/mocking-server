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
import net.liftweb.json.parse
import net.liftweb.json.DefaultFormats
import spray.http.HttpHeader
import spray.http.HttpResponse
import net.liftweb.json.JObject
import net.liftweb.json.JValue
import java.util.Date
import java.net.URLEncoder

case class Header(val header : String, val value : String)
case class Metadata(val status : Int, val header : Option[Set[Header]], val query : Map[String, String])

case class GenericHttpHeader(val name : String, val value : String) extends HttpHeader {
	lazy val lowercaseName = name.toLowerCase()
}

object MetadataFileDirectory {
	private val Pattern = "(.*)\\.metadata$".r

	def findReturnName(metaFilename : String, folder : File) : String = metaFilename match {
		case Pattern(c) => {
			def isValid(file : File) = file.getName.startsWith(c) && !file.getName.endsWith(".metadata")

			val returnFiles = folder.listFiles.filter(isValid).map(_.getPath)
			assert(returnFiles.size == 1, "Invalid number of files to return, found " + returnFiles +
				". Ensure each metadata file only has one associated file")
			returnFiles.head
		}
		case _ => throw new RuntimeException("Failed to find file for metadata file " + metaFilename)
	}
}

class MetadataFileDirectory(val path : File) {
	implicit val formats = DefaultFormats

	def toMetadata(json : JValue) = {
		val headers = (json \ "headers").extractOpt[Set[Header]]
		val status = (json \ "status").extractOpt[Int].getOrElse(200)
		val queryParams = (json \ "query").extract[JObject].values

		assert(!queryParams.isEmpty, "No query parameters for " + path.getPath())
		Metadata(status, headers, queryParams.map(entry => (entry._1, entry._2.toString)).toMap)
	}

	def autoclose[T <: { def close() : Unit }, V](source : T)(f : T => V) : V = {
		import scala.language.reflectiveCalls
		try {
			f(source)
		} finally {
			source.close
		}
	}

	lazy val metadataFiles = path.listFiles().filter(_.getName().endsWith(".metadata")).map(file => {
		(MetadataFileDirectory.findReturnName(file.getName(), path), {
			autoclose(Source.fromFile(file)) {
				source => toMetadata(parse(source.getLines.mkString("")))
			}
		})
	}).toMap
}

class Service extends Actor with HttpService {
	def metadataMaps = new MetadataFileDirectory(new File("c:\\Development\\metadata")).metadataFiles

	val bigIntMarshaller = Marshaller.of[BigInt](ContentType.`text/plain`) { (value, contentType, ctx) =>
		ctx.marshalTo(HttpBody(contentType, value.toString + " "))
	}

	def byteArrayOf(path : String) = {
		val bis = new BufferedInputStream(new FileInputStream(path))
		Stream.continually(bis.read).takeWhile(_ != -1).map(_.toByte).toArray
	}

	def actorRefFactory = context

	def receive = handleTimeouts orElse runRoute(
		(get & path("mocked")) { ctx =>
			val params = ctx.request.queryParams.toSet
			val metadata = metadataMaps.find(entry => entry match {
				case (key, Metadata(_, _, query)) => (query.toSet intersect params).size == query.size
			})

			metadata match {
				case Some((path, entry)) => {
					import spray.http.StatusCode.int2StatusCode
					// TODO: Should eventually be a chunked stream response
					val bytes = byteArrayOf(path)

					val headers = entry match {
						case Metadata(_, Some(headers), _) => headers.map(header => GenericHttpHeader(header.header, header.value)).toList
						case _ => Nil
					}

					ctx.complete(entry.status, headers, bytes)
				}
				case None => {
					// TODO: Allow a default response if its invalid
					println("No mapping for " + params)
					ctx.complete(StatusCodes.NotFound, Nil, "")
				}
			}
		} ~ (get & path("list")) { ctx =>
			ctx.complete(metadataMaps.map(entry => {
				val (filename, metadata) = entry

				val combinedParams = for (
					entry <- metadata.query;
					list = List(entry._1, entry._2)
				) yield list.map(encode).mkString("=")

				"http://" + ctx.request.hostAndPort + "/mocked?" + combinedParams.mkString("&") +
					"\n\t" + filename
			}).mkString("\n\n"))
		})

	def encode = URLEncoder.encode(_ : String, "UTF-8")

	def handleTimeouts : Receive = {
		case Timeout(x : HttpRequest) =>
			sender ! HttpResponse(StatusCodes.InternalServerError, "Request timed out.")
	}
}

object Mainsy extends App with SprayCanHttpServerApp {
	// TODO: Allow port to be set via commandline
	val me = system.actorOf(Props[Service])
	newHttpServer(me) ! Bind(interface = "localhost", port = 8888)
}