package com.getjenny.starchat.analyzer.analyzers
import java.io.File

import javax.script.{Compilable, ScriptEngine, ScriptEngineManager}
import org.scalajs.core.tools.io.{IRFileCache, MemVirtualSerializedScalaJSIRFile, VirtualScalaJSIRFile, WritableMemVirtualJSFile}
import org.scalajs.core.tools.io.IRFileCache.VirtualRelativeIRFile
import org.scalajs.core.tools.linker.{ModuleInitializer, StandardLinker}
import org.scalajs.core.tools.logging.ScalaConsoleLogger

import scala.reflect.io.VirtualDirectory
import scala.reflect.io
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.{Global, Settings}
import scala.util.{Failure, Success, Try}

object ScalaJSAnalyzerBuilder extends AnalyzerAbstractBuilder {

  private[this] val engine: ScriptEngine with Compilable = {
    val manager = new ScriptEngineManager(getClass.getClassLoader)
    manager.getEngineByName("nashorn") match {
      case engine: ScriptEngine with Compilable => engine
      case _ => throw new Exception("can't create the ScriptEngine")
    }
  }

  /**
    * Defines the names of the module and the main method of the ScalaJS script.
    * The script must contain object ScalaJSAnalyzer with main method in order to work.
    */
  private[this] val mainModuleInitializerSeq = Seq(ModuleInitializer
    .mainMethodWithArgs(moduleClassName = "ScalaJSAnalyzer",
      mainMethodName = "main",
      args = Nil
    ))
  private[this] var compiler = new Compiler
  private[this] var processor = new Processor

  def build(script: String, restrictedArgs: Map[String, String] = Map.empty): ScalaJSAnalyzer = {
    /**
      * If and error happens during the compiler or linking process, the module cannot be reused
      * and have to reinitialize.
      */
    val sjsirFiles = Try(compiler.compile(script)) match {
      case Success(value) => value
      case Failure(e) => {
        compiler = new Compiler()
        throw e
      }
    }
    val javaScript = Try(processor.process(sjsirFiles)) match {
      case Success(value) => value
      case Failure(e) => {
        processor = new Processor()
        throw e
      }
    }
    val compiledScript = engine.compile(javaScript)
    new ScalaJSAnalyzer(compiledScript)
  }

  /**
    * Compiles the ScalaJS script into .sjsir files
    */
  private[this] class Compiler {
    private[this] val settings = new Settings()
    settings.embeddedDefaults(getClass.getClassLoader)
    private[this] val reporter = new ConsoleReporter(settings) // use different reporter?
    private[this] val scalaJSLibrary: String = {
      val classPaths: Array[String] = settings.classpath.value.split(":")
      classPaths.find(_.contains("scalajs-library")).getOrElse(throw new Exception("scalajs-library not found"))
    }
    private val linkerLibraries: Seq[VirtualRelativeIRFile] = {
      val irCache: IRFileCache#Cache = new IRFileCache().newCache
      val irContainers: Seq[IRFileCache.IRContainer] = IRFileCache.IRContainer.fromClasspath(Seq(new File(scalaJSLibrary)))
      irCache.cached(irContainers)
    }
    private[this] val compiler: Global = new Global(settings, reporter) {
      override lazy val plugins: List[Plugin] = List(new org.scalajs.core.compiler.ScalaJSPlugin(this))
    }

    private[this] def makeFile(src: Array[Byte], fileName: String = "ScalaJSScript.scala") = {
      val singleFile = new io.VirtualFile(fileName)
      val output     = singleFile.output
      output.write(src)
      output.close()
      singleFile
    }

    def compile(script: String): Seq[VirtualScalaJSIRFile] = {
      val target = new VirtualDirectory("", None)
      settings.outputDirs.setSingleOutput(target)
      val run = new compiler.Run()
      val scalaJSFile = makeFile(script.getBytes("UTF-8"))

      run.compileFiles(List(scalaJSFile))

      if(target.iterator.isEmpty) throw new Exception("The script was probably flawed")

      val sjsirFiles = for {
        x <- target.iterator.to[collection.immutable.Traversable]
        if x.name.endsWith(".sjsir")
      } yield {
        val f = new MemVirtualSerializedScalaJSIRFile(x.path)
        f.content = x.toByteArray
        f: VirtualScalaJSIRFile
      }
      linkerLibraries ++ sjsirFiles.toSeq
    }
  }

  /**
    * Processes the .sjsir files into JavaScript
    */
  private[this] class Processor {
    private[this] val linkerConfig = StandardLinker.Config()
    private[this] val linker = StandardLinker(linkerConfig)
    private[this] val logger = new ScalaConsoleLogger() // use different logger?

    def process(sjsirFiles: Seq[VirtualScalaJSIRFile]): String = {
      val output = WritableMemVirtualJSFile("output.js")
      linker.link(sjsirFiles,
        mainModuleInitializerSeq,
        output,
        logger
      )
      output.content
    }
  }
}
