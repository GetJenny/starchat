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

object ScalaJSAnalyzerBuilder extends AnalyzerAbstractBuilder {

  private val engine: ScriptEngine with Compilable = {
    val manager = new ScriptEngineManager(null)
    manager.getEngineByName("nashorn") match {
      case engine: ScriptEngine with Compilable => engine
    }
  }

  /**
    * Defines the names of the module and the main method of the ScalaJS script.
    * The script must contain object ScalaJSAnalyzer with main method in order to work.
    */
  private val mainModuleInitializerSeq = Seq(ModuleInitializer
    .mainMethodWithArgs(moduleClassName = "ScalaJSAnalyzer",
      mainMethodName = "main",
      args = Nil
    ))
  private var compiler = new Compiler
  private var processor = new Processor

  def build(script: String, restrictedArgs: Map[String, String] = Map.empty): ScalaJSAnalyzer = {
    /**
      * If and error happens during the compiler or linking process, the module cannot be reused
      * and have to reinitialize.
      */
    val sjsirFiles = try{
      compiler.compile(script)
    } catch {
      case e: Throwable => {
        compiler = new Compiler()
        throw e
      }
    }
    val javaScript = try {
      processor.process(sjsirFiles)
    } catch {
      case e: Throwable => {
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
  private class Compiler {
    private val settings = new Settings()
    settings.embeddedDefaults(getClass.getClassLoader)
    private val reporter = new ConsoleReporter(settings) // use different reporter?
    private val scalaJSLibrary: String = {
      val classPaths: Array[String] = settings.classpath.value.split(":")
      classPaths.find(_.contains("scalajs-library")).get
    }
    private val linkerLibraries: Seq[VirtualRelativeIRFile] = {
      val irCache: IRFileCache#Cache = new IRFileCache().newCache
      val irContainers: Seq[IRFileCache.IRContainer] = IRFileCache.IRContainer.fromClasspath(Seq(new File(scalaJSLibrary)))
      irCache.cached(irContainers)
    }
    private val compiler: Global = new Global(settings, reporter) {
      override lazy val plugins: List[Plugin] = List(new org.scalajs.core.compiler.ScalaJSPlugin(this))
    }

    private def makeFile(src: Array[Byte], fileName: String = "ScalaJSScript.scala") = {
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
  private class Processor {
    private val linkerConfig = StandardLinker.Config()
    private val linker = StandardLinker(linkerConfig)
    private val logger = new ScalaConsoleLogger() // use different logger?

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
