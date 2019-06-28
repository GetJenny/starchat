package com.getjenny.starchat.analyzer.analyzers.script_support.scalajs

/**
  * copied and modified from ScalaFiddle
  * https://github.com/scalafiddle/scalafiddle-core/blob/master/compiler-server/src/main/scala/scalafiddle/compiler/LibraryManager.scala
  */

import java.io._
import java.nio.file.Paths

import org.scalajs.core.tools.io.IRFileCache.VirtualRelativeIRFile
import org.scalajs.core.tools.io.{RelativeVirtualFile, _}
import org.slf4j.LoggerFactory

import scala.tools.nsc.io.AbstractFile

class JarEntryIRFile(outerPath: String, val relativePath: String)
  extends MemVirtualSerializedScalaJSIRFile(s"$outerPath:$relativePath")
    with RelativeVirtualFile

class VirtualFlatJarFile(flatJar: FlatJar, ffs: FlatFileSystem) extends VirtualJarFile {
  override def content: Array[Byte] = null
  override def path: String         = flatJar.name
  override def exists: Boolean      = true

  override def sjsirFiles: Seq[VirtualScalaJSIRFile with RelativeVirtualFile] = {
    flatJar.files.filter(_.path.endsWith("sjsir")).map { file =>
      val content = ffs.load(flatJar, file.path)
      new JarEntryIRFile(flatJar.name, file.path).withContent(content).withVersion(Some(path))
    }
  }
}

/**
  * Loads the jars that make up the classpath of the scala-js-fiddle
  * compiler and re-shapes it into the correct structure to satisfy
  * scala-compile and scalajs-tools
  */
object LibraryManager {
  private[this] val log = LoggerFactory.getLogger(getClass)

  private[this] val baseLibs = Seq(
    s"/scala-library-${Config.scalaVersion}.jar",
    s"/scalajs-library_${Config.scalaMainVersion}-${Config.scalaJSVersion}.jar")

  private[this] val commonJars = {
    val jarFiles = baseLibs.map { name =>
      val stream = getClass.getResourceAsStream(name)
      log.debug(s"Loading resource $name")
      if (stream == null) {
        throw new Exception(s"Classpath loading failed, jar $name not found")
      }
      name -> stream
    }.seq

    val bootFiles = for {
      prop <- Seq("sun.boot.class.path")
      path <- System.getProperty(prop).split(System.getProperty("path.separator"))
      vfile = scala.reflect.io.File(path)
      if vfile.exists && !vfile.isDirectory
    } yield {
      val name = "system/" + path.split(File.separatorChar).last
      log.debug(s"Loading resource $name")
      name -> vfile.inputStream()
    }
    jarFiles ++ bootFiles
  }

  private[this] def loadFiles = {
    val ffs    = FlatFileSystem.build(Paths.get(Config.libCache), commonJars)
    val absffs = new AbstractFlatFileSystem(ffs)

    val commonJarFlatFiles = commonJars.map(jar => (jar._1, absffs.roots(jar._1))).toMap

    val commonLibs = commonJars.map { case (jar, _) => jar -> commonJarFlatFiles(jar) }

    (commonLibs, ffs)

  }

  private[this] val (commonLibs, ffs) = loadFiles
  /**
    * The loaded files shaped for Scala-Js-Tools to use
    */
  private[this] def lib4linker(file: AbstractFlatJar) = {
    val jarFile = new VirtualFlatJarFile(file.flatJar, ffs)
    IRFileCache.IRContainer.Jar(jarFile)
  }

  /**
    * In memory cache of all the jars used in the compiler. This takes up some
    * memory but is better than reaching all over the filesystem every time we
    * want to do something.
    */
  private[this] val commonLibraries4compiler: Seq[AbstractFlatDir] = commonLibs.map { case (name, data) => data.root }.seq

  /**
    * In memory cache of all the jars used in the linker.
    */
  private[this] val commonLibraries4linker = commonLibs.map { case (name, file) => lib4linker(file) }


  def compilerLibraries: Seq[AbstractFile] = {
    commonLibraries4compiler
  }

  private[this] val irCache = new IRFileCache

  def linkerLibraries: Seq[VirtualRelativeIRFile] = {
    val loadedJars = commonLibraries4linker
    val cache      = irCache.newCache
    cache.cached(loadedJars)
  }
}

