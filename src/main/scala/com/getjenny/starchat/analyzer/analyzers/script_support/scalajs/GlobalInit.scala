package com.getjenny.starchat.analyzer.analyzers.script_support.scalajs

/**
  * copied and modified from ScalaFiddle
  * https://github.com/scalafiddle/scalafiddle-core/blob/master/compiler-server/src/main/scala-2.12/scalafiddle/compiler/GlobalInitCompat.scala
  */

import java.net.{URL, URLClassLoader}

import scala.collection.mutable
import scala.reflect.io
import scala.tools.nsc.classpath.{AggregateClassPath, FileUtils, VirtualDirectoryClassPath}
import scala.tools.nsc.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.{Global, Settings}
import scala.util.Try

object GlobalInit {
  private[this] def inMemClassloader(libs: Seq[io.AbstractFile]): ClassLoader = {
    new URLClassLoader(new Array[URL](0), this.getClass.getClassLoader) {
      private val classCache = mutable.Map.empty[String, Option[Class[_]]]

      override def findClass(name: String): Class[_] = {
        def findClassInLibs(): Option[AbstractFile] = {
          val parts = name.split('.')
          libs
            .map(dir => {
              Try {
                parts
                  .dropRight(1)
                  .foldLeft[AbstractFile](dir)((parent, next) => parent.lookupName(next, directory = true))
                  .lookupName(parts.last + ".class", directory = false)
              } getOrElse null
            })
            .find(_ != null)
        }

        val res = classCache.getOrElseUpdate(
          name,
          findClassInLibs().map { f =>
            val data = f.toByteArray
            this.defineClass(name, data, 0, data.length)
          }
        )
        res match {
          case None =>
            println("Not Found Class " + name)
            throw new ClassNotFoundException()
          case Some(cls) =>
            cls
        }
      }

      override def close() = {}
    }
  }

  private[this] final def lookupPath(base: AbstractFile)(pathParts: Seq[String], directory: Boolean): AbstractFile = {
    var file: AbstractFile = base
    for (dirPart <- pathParts.init) {
      file = file.lookupName(dirPart, directory = true)
      if (file == null)
        return null
    }

    file.lookupName(pathParts.last, directory = directory)
  }

  private[this] def buildClassPath(absFile: AbstractFile) =
    new VirtualDirectoryClassPath(new VirtualDirectory(absFile.name, None) {
      override def iterator = absFile.iterator

      override def lookupName(name: String, directory: Boolean) = absFile.lookupName(name, directory)

      override def subdirectoryNamed(name: String) = absFile.subdirectoryNamed(name)
    }) {
      override def getSubDir(packageDirName: String): Option[AbstractFile] = {
        Option(lookupPath(absFile)(packageDirName.split('/'), directory = true))
      }

      override def findClassFile(className: String): Option[AbstractFile] = {
        val relativePath = FileUtils.dirPath(className) + ".class"
        Option(lookupPath(absFile)(relativePath.split('/'), directory = false))
      }
    }

  def initGlobal(settings: Settings, reporter: Reporter, libs: Seq[io.AbstractFile]): Global = {

    val cp = new AggregateClassPath(libs.map(buildClassPath))
    val cl = inMemClassloader(libs)

    new Global(settings, reporter) { g =>

      override lazy val plugins: List[Plugin] = List(new org.scalajs.core.compiler.ScalaJSPlugin(this))

      override lazy val platform: ThisPlatform = new GlobalPlatform {
        override val global    = g
        override val settings  = g.settings
        override def classPath = cp
      }

      override lazy val analyzer = new {
        val global: g.type = g
      } with Analyzer {
        override def findMacroClassLoader() = cl
      }
    }
  }
}

