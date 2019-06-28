package com.getjenny.starchat.analyzer.analyzers.script_support.scalajs

/**
  * This is copied from ScalaFiddle
  * https://github.com/scalafiddle/scalafiddle-core/blob/master/compiler-server/src/main/scala/scalafiddle/compiler/cache/FlatFileSystem.scala
  */

import java.io.{FileOutputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipInputStream}

import com.google.common.io.Files
import org.slf4j.LoggerFactory
import org.xerial.snappy.Snappy
import upickle.default._
import xerial.larray._
import xerial.larray.mmap.MMapMode

import scala.io.Source
import scala.reflect.io.Streamable
//import scala.scalajs.niocharset.StandardCharsets

case class FlatFile(path: String, offset: Long, compressedSize: Int, origSize: Int)

case class FlatJar(name: String, files: Seq[FlatFile])

class FlatFileSystem(data: MappedLByteArray, val jars: Seq[FlatJar], val index: Map[String, FlatFile]) {

  def exists(path: String) = index.contains(path)

  def load(flatJar: FlatJar, path: String): Array[Byte] = {
    load(jars.find(_.name == flatJar.name).get.files.find(_.path == path).get)
  }

  def load(path: String): Array[Byte] = {
    load(index(path))
  }

  def load(file: FlatFile): Array[Byte] = {
    val address = data.address + file.offset
    val content = LArray.of[Byte](file.origSize).asInstanceOf[RawByteArray[Byte]]

    Snappy.rawUncompress(address, file.compressedSize, content.address)
    val bytes = Array.ofDim[Byte](file.origSize)
    content.writeToArray(0, bytes, 0, file.origSize)
    content.free
    bytes
  }

  def filter(f: Set[String]): FlatFileSystem = {
    val newJars = jars.filter(j => f.contains(j.name))
    new FlatFileSystem(data, newJars, FlatFileSystem.createIndex(newJars))
  }
}

object FlatFileSystem {
  val log = LoggerFactory.getLogger(getClass)

  def apply(location: Path): FlatFileSystem = {
    location.toFile.mkdirs()
    val jars                         = readMetadata(location)
    val index: Map[String, FlatFile] = createIndex(jars)
    val data                         = LArray.mmap(location.resolve("data").toFile, MMapMode.READ_ONLY)
    new FlatFileSystem(data, jars, index)
  }

  private def createIndex(jars: Seq[FlatJar]): Map[String, FlatFile] = {
    jars.flatMap(_.files.map(file => (file.path, file)))(collection.breakOut)
  }

  private[this] def readMetadata(location: Path): Seq[FlatJar] = {
    read[Seq[FlatJar]](Source.fromFile(location.resolve("index.json").toFile, "UTF-8").getLines.mkString)
  }

  private[this] val validExtensions = Set("class", "sjsir")
  private[this] def validFile(entry: ZipEntry) = {
    !entry.isDirectory && validExtensions.contains(Files.getFileExtension(entry.getName))
  }

  def build(location: Path, jars: Seq[(String, InputStream)]): FlatFileSystem = {
    // if metadata already exists, read it in
    val existingJars = if (location.resolve("index.json").toFile.exists()) readMetadata(location) else Seq.empty[FlatJar]

    val newJars = jars.filterNot(p => existingJars.exists(_.name == p._1))

    // make location path
    location.toFile.mkdirs()

    val dataFile = location.resolve("data").toFile
    val fos      = new FileOutputStream(dataFile, true)
    var offset   = dataFile.length()

    // read through all new JARs, append contents to data and create metadata
    val addedJars = newJars.map { jarPath =>
      val name = jarPath._1
      log.debug(s"Extracting JAR $name")
      val fis       = jarPath._2
      val jarStream = new ZipInputStream(fis)
      val entries = Iterator
        .continually(jarStream.getNextEntry)
        .takeWhile(_ != null)
        .filter(validFile)

      val files = entries.map { entry =>
        // read and compress the file
        val content    = Streamable.bytes(jarStream)
        val compressed = Snappy.compress(content)
        fos.write(compressed)
        val ff = FlatFile(entry.getName, offset, compressed.length, content.length)
        offset += compressed.length
        ff
      }.toList
      jarStream.close()
      FlatJar(name, files)
    }
    fos.close()

    val finalJars = existingJars ++ addedJars
    val json      = write(finalJars)
    Files.write(json, location.resolve("index.json").toFile, StandardCharsets.UTF_8)

    val data = LArray.mmap(location.resolve("data").toFile, MMapMode.READ_ONLY)
    new FlatFileSystem(data, finalJars, createIndex(finalJars))
  }
}
