package com.getjenny.starchat.analyzer.analyzers.script_support.scalajs


import com.getjenny.starchat.analyzer.analyzers.AbstractAnalyzerBuilder
import javax.script.{Compilable, ScriptEngine, ScriptEngineManager}
import org.scalajs.core.tools.io.IRFileCache.VirtualRelativeIRFile
import org.scalajs.core.tools.io.{MemVirtualSerializedScalaJSIRFile, VirtualScalaJSIRFile, WritableMemVirtualJSFile}
import org.scalajs.core.tools.linker.{ModuleInitializer, StandardLinker}
import org.scalajs.core.tools.logging.ScalaConsoleLogger
import org.scalajs.core.tools.sem.Semantics

import scala.reflect.io
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Settings
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.StoreReporter
import scala.util.{Failure, Success, Try}

class CompilationFailedException(val messages: Iterable[(Int, String)])
  extends Exception(messages.map{ case (row, cause) => row + ". " + cause}.mkString("\n"))

object ScalaJSAnalyzerBuilder extends AbstractAnalyzerBuilder {

  private[this] val linkerLibraries: Seq[VirtualRelativeIRFile] = LibraryManager.linkerLibraries
  private[this] val compilerLibraries: Seq[AbstractFile] = LibraryManager.compilerLibraries

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
      case Failure(e) =>
        compiler = new Compiler()
        throw e
    }
    val javaScript = Try(processor.process(sjsirFiles)) match {
      case Success(value) => value
      case Failure(e) =>
        processor = new Processor()
        throw e
    }
    val compiledScript = engine.compile(javaScript)
    new ScalaJSAnalyzer(compiledScript, restrictedArgs)
  }

  /**
    * Compiles the ScalaJS script into .sjsir files
    */
  private[this] class Compiler {
    private[this] val settings = new Settings()
    settings.processArgumentString("-Ydebug -Ypartial-unification -Ylog-classpath")
    private[this] val reporter = new StoreReporter()

    private[this] val compiler = GlobalInit.initGlobal(settings, reporter, compilerLibraries)

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
      compiler.reporter.reset()
      val run = new compiler.Run()
      val scalaJSFile = makeFile(script.getBytes("UTF-8"))

      run.compileFiles(List(scalaJSFile))

      if(reporter.hasWarnings || reporter.hasErrors){
        val errors = reporter.infos.map(info => (info.pos.line, info.msg))
        throw new CompilationFailedException(errors)
      }
      if(target.iterator.isEmpty) throw new Exception("Compiler's output was empty")

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
      .withSemantics(Semantics.Defaults.optimized)
      .withSourceMap(false)
      .withClosureCompilerIfAvailable(true)
      .withOptimizer(true)
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

