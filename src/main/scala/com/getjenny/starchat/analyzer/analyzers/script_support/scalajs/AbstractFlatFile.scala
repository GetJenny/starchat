package com.getjenny.starchat.analyzer.analyzers.script_support.scalajs

/**
  * This is copied from ScalaFiddle
  * https://github.com/scalafiddle/scalafiddle-core/blob/master/compiler-server/src/main/scala/scalafiddle/compiler/cache/AbstractFlatFile.scala
  */

import java.io.{ByteArrayInputStream, File, InputStream, OutputStream}

import scalaz.Scalaz._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.io._
import scala.tools.nsc.io.AbstractFile

class AbstractFlatFile(flatFile: FlatFile, flatJar: FlatJar, ffs: FlatFileSystem) extends AbstractFile {
  override val path                    = flatFile.path
  override val name: String            = path.split('/').last
  override def absolute: AbstractFile  = this
  override def container: AbstractFile = NoAbstractFile
  override def file: File              = unsupported()
  override def create(): Unit          = unsupported()
  override def delete(): Unit          = unsupported()
  override def isDirectory: Boolean    = false
  val lastModified: Long               = System.currentTimeMillis
  override def input: InputStream = {
    new ByteArrayInputStream(ffs.load(flatFile.path))
  }

  override def toByteArray: Array[Byte] = {
    ffs.load(flatJar, flatFile.path)
  }

  override def output: OutputStream                                                = unsupported()
  override def iterator: Iterator[AbstractFile]                                    = Iterator.empty
  override def lookupName(name: String, directory: Boolean): AbstractFile          = unsupported()
  override def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()

  override def toString(): String = s"AFF($name)"
}

class AbstractFlatDir(val path: String, val children: ArrayBuffer[AbstractFile] = ArrayBuffer.empty) extends AbstractFile {
  private[this] lazy val files: Map[String, AbstractFile] = children.map(c => c.name -> c)(collection.breakOut)
  override val name: String                         = path.split('/').last
  override def absolute: AbstractFile               = this
  override def container: AbstractFile              = NoAbstractFile
  override def file: File                           = unsupported()
  override def create(): Unit                       = unsupported()
  override def delete(): Unit                       = unsupported()
  override def isDirectory: Boolean                 = true
  val lastModified: Long                            = System.currentTimeMillis
  override def input: InputStream                   = unsupported()
  override def output: OutputStream                 = unsupported()
  override def iterator: Iterator[AbstractFile] = {
    children.iterator
  }
  override def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = {
    unsupported()
  }

  override def lookupName(name: String, directory: Boolean): AbstractFile = {
    files.get(name).filter(_.isDirectory === directory).orNull
  }

  override def toString(): String = s"AFD($path, ${children.map(_.toString).mkString(",")})\n\n"
}

class AbstractFlatJar(val flatJar: FlatJar, ffs: FlatFileSystem) {
  val root = new AbstractFlatDir(flatJar.name, ArrayBuffer.empty)
  val dirs = mutable.HashMap[String, AbstractFlatDir]("" -> root)

  build()

  def build(): Unit = {
    def findParent(path: Seq[String]): AbstractFlatDir = {
      val dir = path.mkString("/")
      dirs.get(dir) match {
        case Some(absDir) =>
          absDir
        case None =>
          val parent = findParent(path.dropRight(1))
          val newDir = new AbstractFlatDir(dir.drop(1))
          dirs.put(dir, newDir)
          parent.children.append(newDir)
          newDir
      }
    }

    flatJar.files.foreach { file =>
      val path    = "" +: file.path.split('/')
      val parent  = findParent(path.dropRight(1))
      val newFile = new AbstractFlatFile(file, flatJar, ffs)
      parent.children.append(newFile)
    }
  }
}

class AbstractFlatFileSystem(ffs: FlatFileSystem) {
  val roots = ffs.jars.map(jar => jar.name -> new AbstractFlatJar(jar, ffs)).toMap
}

