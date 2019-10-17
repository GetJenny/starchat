package com.getjenny.starchat.analyzer.analyzers.script_support.scalajs


import com.getjenny.starchat.analyzer.analyzers.AbstractAnalyzerBuilder
import javax.script.{Compilable, ScriptEngine, ScriptEngineManager}
import org.scalajs.core.tools.io.IRFileCache.VirtualRelativeIRFile
import org.scalajs.core.tools.io.{MemVirtualSerializedScalaJSIRFile, VirtualScalaJSIRFile, WritableMemVirtualJSFile}
import org.scalajs.core.tools.linker.{ModuleInitializer, StandardLinker}
import org.scalajs.core.tools.logging.ScalaConsoleLogger

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
      case _ => throw new Exception("Unable to create the ScriptEngine")
    }
  }

  private[this] var compiler = new Compiler
  private[this] var processor = new Processor

  def build(script: String, restrictedArgs: Map[String, String] = Map.empty): ScalaJSAnalyzer = {

    def template(script: String): String =
      f"""
        |import scalajs.js
        |import scala.scalajs.js.Dynamic.{global => g}
        |
        |object ScalaJSAnalyzer {
        |  def main() = {
        |    g.analyzer = (sentence: String, analyzersDataInternal: js.Dynamic, restrictedArgs: js.Dynamic) => {
        |      $script
        |    }
        |  }
        |}
        |""".stripMargin

    val scriptToCompile = template(script)

    /**
      * If an error occurs during the compiler or linking process, the module cannot be reused
      * and have to reinitialize.
      *
      * update: 11.11.2019 there was a pull request merged in ScalaJS-project which allows linker to be reset
      * after an exception. Consider updating ScalaJS version when it's ready.
      */
    val sjsirFiles = Try(compiler.compile(scriptToCompile)) match {
      case Success(value) => value
      case Failure(e) =>
        compiler = new Compiler()
        throw e
    }
    val moduleInitializer =  ModuleInitializer.mainMethod("ScalaJSAnalyzer","main")
    val javaScript = Try(processor.process(sjsirFiles, moduleInitializer: ModuleInitializer)) match {
      case Success(value) => value
      case Failure(e) =>
        processor = new Processor()
        throw e
    }

    val compiledScript = engine.compile(javaScript)
    new ScalaJSAnalyzer(compiledScript, restrictedArgs)
  }

  /**
    * Compiles the ScalaJS script into .sjsir files. Files will be consumed by linker.
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
      .withSourceMap(false)
      /*
      .withSemantics(Semantics.Defaults.optimized)
      .withClosureCompilerIfAvailable(true)
       */

    private[this] val linker = StandardLinker(linkerConfig)
    private[this] val logger = new ScalaConsoleLogger() // use different logger?

    def process(sjsirFiles: Seq[VirtualScalaJSIRFile], moduleInitializer: ModuleInitializer): String = {
      val output = WritableMemVirtualJSFile("output.js")
      linker.link(sjsirFiles,
        Seq(moduleInitializer),
        output,
        logger
      )
      output.content
    }
  }
}

