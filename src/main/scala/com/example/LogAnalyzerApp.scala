package com.example

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.io.{File, FileInputStream, BufferedReader, InputStreamReader}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardCopyOption}

object LogAnalyzerApp {

  case class LogStats(
    totalLines: Long,
    errorCount: Long,
    warningCount: Long,
    infoCount: Long,
    topErrors: Map[String, Int],
    timeRange: Option[(String, String)]
  )

  def main(args: Array[String]): Unit = {
    val server = HttpServer.create(new InetSocketAddress(8080), 0)
    
    // Обработчик главной страницы
    server.createContext("/", new HttpHandler {
      def handle(ex: HttpExchange): Unit = {
        val path = ex.getRequestURI.getPath
        if (path == "/" || path == "/index.html") {
          try {
            val html = scala.io.Source.fromResource("index.html").mkString
            sendResponse(ex, 200, html, "text/html")
          } catch {
            case e: Exception => 
              e.printStackTrace()
              sendResponse(ex, 500, s"Error loading index.html: ${e.getMessage}", "text/plain")
          }
        } else {
          sendResponse(ex, 404, "Not Found", "text/plain")
        }
      }
    })

    // API: Анализ текста
    server.createContext("/api/analyze/text", new HttpHandler {
      def handle(ex: HttpExchange): Unit = {
        if (ex.getRequestMethod == "POST") {
          val body = new String(ex.getRequestBody.readAllBytes(), StandardCharsets.UTF_8)
          val stats = analyzeText(body)
          sendJson(ex, 200, stats)
        } else {
          sendResponse(ex, 405, "Method Not Allowed", "text/plain")
        }
      }
    })

    // API: Анализ файла
    server.createContext("/api/analyze/file", new HttpHandler {
      def handle(ex: HttpExchange): Unit = {
        if (ex.getRequestMethod == "POST") {
          val tempFile = File.createTempFile("log-", ".tmp")
          tempFile.deleteOnExit()
          try {
            Files.copy(ex.getRequestBody, tempFile.toPath, StandardCopyOption.REPLACE_EXISTING)
            val stats = analyzeFile(tempFile)
            sendJson(ex, 200, stats)
          } catch {
            case e: Exception => 
              e.printStackTrace()
              sendResponse(ex, 500, e.getMessage, "text/plain")
          } finally {
            tempFile.delete()
          }
        } else {
          sendResponse(ex, 405, "Method Not Allowed", "text/plain")
        }
      }
    })

    // API: Проверка здоровья
    server.createContext("/api/health", new HttpHandler {
      def handle(ex: HttpExchange): Unit = {
        sendJson(ex, 200, Map("status" -> "ok"))
      }
    })

    server.setExecutor(null)
    server.start()
    println("✅ Server started at http://localhost:8080")
  }

  def analyzeText(text: String): LogStats = {
    val stream = new java.io.ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8))
    val reader = new BufferedReader(new InputStreamReader(stream, StandardCharsets.UTF_8))
    try {
      analyzeStream(reader)
    } finally {
      reader.close()
    }
  }

  def analyzeFile(file: File): LogStats = {
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8))
    try {
      analyzeStream(reader)
    } finally {
      reader.close()
    }
  }

  def analyzeStream(reader: BufferedReader): LogStats = {
    var totalLines = 0L
    var errorCount = 0L
    var warningCount = 0L
    var infoCount = 0L
    val errorMessages = scala.collection.mutable.Map[String, Int]()
    var firstTs: Option[String] = None
    var lastTs: Option[String] = None

    var line = reader.readLine()
    while (line != null) {
      totalLines += 1
      parseLine(line) match {
        case Some(entry) =>
          if (entry.timestamp.nonEmpty) {
            if (firstTs.isEmpty) firstTs = Some(entry.timestamp)
            lastTs = Some(entry.timestamp)
          }
          entry.level match {
            case "ERROR" | "ERR" | "FATAL" =>
              errorCount += 1
              errorMessages.update(entry.message, errorMessages.getOrElse(entry.message, 0) + 1)
            case "WARN" | "WARNING" => warningCount += 1
            case "INFO" => infoCount += 1
            case _ =>
          }
        case None =>
      }
      line = reader.readLine()
    }

    LogStats(totalLines, errorCount, warningCount, infoCount,
      errorMessages.toSeq.sortBy(-_._2).take(10).toMap,
      firstTs.zip(lastTs).headOption)
  }

  private val logPattern = """(\d{4}-\d{2}-\d{2}[T\s]\d{2}:\d{2}:\d{2}[.,]?\d*)\s*\[?(\w+)\]?\s*(.*)""".r
  case class LogEntry(timestamp: String, level: String, message: String)

  def parseLine(line: String): Option[LogEntry] = line.trim match {
    case logPattern(ts, lvl, msg) => Some(LogEntry(ts.trim, lvl.toUpperCase.trim, msg.trim))
    case _ if line.trim.nonEmpty => Some(LogEntry("", "UNKNOWN", line.trim))
    case _ => None
  }

  def sendJson(ex: HttpExchange, code: Int, stats: LogStats): Unit = {
    val errorsJson = stats.topErrors.map { case (k, v) => s""""$k":$v""" }.mkString("{", ",", "}")
    val timeJson = stats.timeRange.map(t => s"""["${t._1}","${t._2}"]""").getOrElse("null")
    val json = s"""{"totalLines":${stats.totalLines},"errorCount":${stats.errorCount},"warningCount":${stats.warningCount},"infoCount":${stats.infoCount},"topErrors":$errorsJson,"timeRange":$timeJson}"""
    sendResponse(ex, code, json, "application/json")
  }

  def sendJson(ex: HttpExchange, code: Int, map: Map[String, String]): Unit = {
    val json = map.map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")
    sendResponse(ex, code, json, "application/json")
  }

  // ИСПРАВЛЕННЫЙ МЕТОД - сначала конвертируем в байты, потом считаем длину
  def sendResponse(ex: HttpExchange, code: Int, body: String, contentType: String): Unit = {
    val bodyBytes = body.getBytes(StandardCharsets.UTF_8)
    ex.getResponseHeaders.set("Content-Type", contentType)
    ex.getResponseHeaders.set("Access-Control-Allow-Origin", "*")
    ex.getResponseHeaders.set("Content-Length", bodyBytes.length.toString)
    ex.sendResponseHeaders(code, bodyBytes.length)
    ex.getResponseBody.write(bodyBytes)
    ex.getResponseBody.close()
  }
}