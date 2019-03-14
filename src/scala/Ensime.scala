//
// Scaled Scala Mode - support for editing Scala code
// https://github.com/scaled/scala-mode/blob/master/LICENSE

package scaled.project

import com.google.common.cache.LoadingCache
import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import scaled._
import scaled.pacman._
import scaled.util.Close

object EnsimeConfig {

  val DotEnsime = ".ensime"

  // SExpr AST and parser, because Emacs
  sealed trait SExp {
    def asString :Option[String] = None
    def asList :Seq[SExp] = Seq()
    def asMap :Map[String,SExp] = Map()
    def print (indent :String = "", out :PrintWriter) :Unit = {}
  }
  case class SAtom (value :String) extends SExp {
    override def print (indent :String, out :PrintWriter) = out.print(value)
    override def toString = value
  }
  case class SString (value :String) extends SExp {
    override def asString = Some(value)
    override def print (indent :String, out :PrintWriter) = out.print(toString)
    override def toString = '"' + value + '"'
  }
  case class SList (elems :Seq[SExp]) extends SExp {
    override def asList = elems
    override def asMap = Map() ++ elems.grouped(2).flatMap {
      case Seq(SAtom(key), value) => Some(key -> value)
      case group => println(s"Invalid sexp-map pair: $group") ; None
    }
    override def print (indent :String, out :PrintWriter) = {
      out.print("(")
      if (elems.size > 0) elems(0).print(indent, out)
      if (elems.size > 1) elems.drop(1) foreach { elem =>
        out.print(" ")
        elem.print(indent, out)
      }
      out.print(")")
    }
    override def toString = elems.mkString("(", ",", ")")
  }
  case class SMap (elems :Map[String,SExp]) extends SExp {
    override def asList = elems.flatMap((k, v) => Seq(SAtom(k), v))
    override def asMap = elems
    override def print (indent :String, out :PrintWriter) = {
      out.print("(")
      val nindent = indent + "  "
      elems foreach { (k, v) =>
        out.println()
        out.print(nindent)
        out.print(k)
        out.print(" ")
        v.print(nindent, out)
      }
      out.print(")")
    }
  }
  case object SEnd extends SExp // used during parsing

  def parseSExp (path :Path) :SExp = {
    val in = Files.newBufferedReader(path)

    // hacky way to allow parseSExp to back up when it sees ')'
    var unread = 0.toChar
    def read :Char = unread match {
      case 0 => in.read match {
        case -1 => throw new IllegalStateException(s"Unexpected EOF")
        case ic => ic.toChar
      }
      case c => try c finally unread = 0.toChar
    }

    def parseSList (accum :Seq.Builder[SExp]) :SList = parseSExp match {
      case SEnd => SList(accum.build)
      case sexp => accum += sexp ; parseSList(accum)
    }

    def parseSString :SString = {
      val buffer = new java.lang.StringBuilder
      var escape = false ; var done = false
      do {
        val c = read
        if (escape) buffer.append(c)
        else if (c == '\\') escape = true
        else if (c == '"') done = true
        else buffer.append(c)
      } while (!done)
      SString(buffer.toString)
    }

    def parseSAtom (first :Char) = {
      val buffer = new java.lang.StringBuilder
      buffer.append(first)
      var done = false
      do {
        val c = read
        if (c == ')') { unread = c ; done = true } // oops, back up!
        else if (c == ' ' || c == '\n') done = true
        else buffer.append(c)
      } while (!done)
      SAtom(buffer.toString)
    }

    def parseComment :SExp = in.read.toChar match {
      case '\n' => parseSExp
      case -1   => SAtom("")
      case _    => parseComment
    }

    def parseSExp :SExp = read match {
      case ';'      => parseComment
      case '('      => parseSList(Seq.builder())
      case ')'      => SEnd
      case '"'      => parseSString
      case ' '|'\n' => parseSExp
      case c        => parseSAtom(c)
    }

    try parseSExp
    finally in.close()
  }

  abstract class DataConfig (val data :Map[String,SExp]) {
    def list (key :String) = data.get(key).map(_.asList) || Seq()
    def string (key :String) = data.get(key).flatMap(_.asString)
    def strings (key :String) = list(key).flatMap(_.asString)
    def paths (key :String) = strings(key).map(dir => Paths.get(dir))
  }

  class EnsimeId (data :Map[String,SExp]) extends DataConfig(data) {
    def project = string(":project") || "unknown"
    def config = string(":config") || "unknown"
    def module :String = s"$project:$config"
    def testModule :Option[String] = if (config == "compile") Some(s"$project:test") else None
  }
  class EnsimeProject (data :Map[String,SExp]) extends DataConfig(data) {
    val id = new EnsimeId(data.get(":id").map(_.asMap) || Map())
    def name = if (id.config == "compile") id.project else s"${id.project}-${id.config}"
  }

  class Config (path :Path, data :Map[String,SExp]) extends DataConfig(data) {
    if (data.isEmpty) println(s"$path does not appear to contain sexp-map data?")
    val projects = list(":projects").map(pd => new EnsimeProject(pd.asMap))
  }

  /** A cache from `Path` to `Config` for `.ensime` files. */
  val configCache :LoadingCache[Path,Config] =
    Mutable.cacheMap(path => new Config(path, parseSExp(path).asMap))

  def main (args :Array[String]) {
    Seq.from(args) flatMap(path => configCache.get(Paths.get(path)).projects) foreach { proj =>
      println(proj.name)
      proj.data foreach { ent => println(s" ${ent._1} $${ent._2}") }
    }
  }
}

object Ensime {
  import EnsimeConfig._

  @Plugin(tag="project-root")
  class EnsimeRootPlugin extends RootPlugin.File(EnsimeConfig.DotEnsime) {
    override protected def createRoot (paths :List[Path], path :Path) = {
      val sentinel = paths.head
      val configFile = path.resolve(DotEnsime)
      val config = configCache.get(configFile)
      // find the project or subproject that contains the sentinel file
      config.projects.find(_.paths(":sources").exists(sentinel startsWith _)) match {
        case Some(proj) => Project.Root(path, proj.id.module)
        case          _ => Project.Root(path)
      }
    }
  }

  class EnsimeDepends (project :Project, enproj :EnsimeProject) extends Depends(project) {
    import Project._

    val rootPath = project.root.path
    val moduleDeps = enproj.list(":depends").map(d => new EnsimeId(d.asMap))

    // if we have no other depends (we're a leaf module) add an implicit depend on the JDK
    // (TODO: would be nice to know which version to use; also implicit dep on scala-library?)
    lazy val ids = if (moduleDeps.isEmpty) Seq(PlatformId(JavaPlatform, JDK.thisJDK.majorVersion))
    else {
      val seen = new java.util.HashSet[Id]()
      val depIds = Seq.builder[Id]()
      def add (id :Id) :Unit = if (seen.add(id)) depIds += id
      for (id <- moduleDeps) add(RootId(rootPath, id.module))
      for (id <- moduleDeps ;
           depId <- project.pspace.projectFor(Root(rootPath, id.module)).depends.ids) add(depId)
      depIds.build()
    }

    def depProjs :Seq[Project] = ids.flatMap(id => project.pspace.projectFor(id))

    def compileDeps = enproj.paths(":targets") ++ enproj.paths(":library-jars")
    def runtimeDeps = Seq() // TODO: use runtime-deps from :subprojects?

    // TODO: we should maybe filter out repeats?
    def buildClasspath :Seq[Path] = compileDeps ++ depProjs.flatMap(_.depends match {
      case endeps :EnsimeDepends => endeps.compileDeps
      case _                     => Seq()
    })
    def execClasspath :Seq[Path] = runtimeDeps ++ buildClasspath
  }

  @Plugin(tag="project-resolver")
  class EnsimeResolverPlugin extends ResolverPlugin {
    override def metaFiles (root :Project.Root) = Seq(root.path.resolve(DotEnsime))
    override def readdComponents (project :Project) {
      configCache.invalidate(project.root.path.resolve(DotEnsime))
      addComponents(project)
    }
    override def addComponents (project :Project) {
      val rootPath = project.root.path
      val encfg = configCache.get(rootPath.resolve(DotEnsime))
      val enproj = encfg.projects.find(_.id.module == project.root.module) || encfg.projects.head

      // add a filer component with custom ignores
      val igns = Ignorer.stockIgnores
      // ignore the SBT project build directories
      val projectDir = rootPath.resolve("project")
      igns += Ignorer.ignorePath(projectDir.resolve("target"), rootPath)
      igns += Ignorer.ignorePath(projectDir.resolve("project"), rootPath)
      // ignore the target directories for *all* modules (since we all share the same root)
      encfg.projects.foreach { proj =>
        proj.paths(":targets") foreach { target => igns += Ignorer.ignorePath(target, rootPath) }
      }
      project.addComponent(classOf[Filer], new DirectoryFiler(project, igns))

      Option(enproj.paths(":sources")).foreach { srcs =>
        project.addComponent(classOf[Sources], new Sources(srcs))
      }

      val depends = new EnsimeDepends(project, enproj)
      project.addComponent(classOf[Depends], depends)

      // a hack to find the 'target' directory, given an SBT classes directory like
      // target/scala-2.12/classes
      def findTarget (path :Path, orig :Path) :Path =
        if (path == null) orig
        else if (path.getFileName.toString == "target") path
        else findTarget(path.getParent, orig)

      val targets = enproj.paths(":targets")
      val classesDir = targets.head
      val java = new JavaComponent(project) {
        def classes = targets
        def buildClasspath = depends.buildClasspath
        def execClasspath = depends.execClasspath
      }
      project.addComponent(classOf[JavaComponent], java)
      java.addTesters()

      val scalaVers = encfg.string(":scala-version") || "2.12.4"
      project.addComponent(classOf[Compiler], new ScalaCompiler(project, java) {
        override def javacOpts = enproj.strings(":javac-options")
        override def scalacOpts = enproj.strings(":scalac-options").
          // filter out this arg that ensime-sbt helpfully adds;
          // it causes scalac to freak out about macros not being expanded
          filter(_ != "-Ymacro-expand:discard")
        override val targetDir = findTarget(classesDir, classesDir)
        override def outputDir = classesDir
        override def scalacVers = scalaVers
        // override protected def willCompile () = copyResources()
      })

      val oldMeta = project.metaV()
      val testRoot = enproj.id.testModule.map(Project.Root(rootPath, _))
      project.metaV() = oldMeta.copy(name = enproj.name, testRoot=testRoot)
    }
  }

  @Plugin(tag="langserver")
  class EnsimeLangPlugin extends LangPlugin {
    def suffs (root :Project.Root) = Set("scala")
    def canActivate (root :Project.Root) = Files.exists(root.path.resolve(DotEnsime))
    def createClient (proj :Project) = Future.success(
      new EnsimeLangClient(proj.metaSvc, proj.root.path))
  }

  // this is the short-lived "Hey, Ensime will provide its own LSP client" binding,
  // which alas never became robust enough to work
  def ensimeServerCmd (metaSvc :MetaService, root :Path) = {
    val config = configCache.get(root.resolve(DotEnsime))
    val compilerJars = config.strings(":scala-compiler-jars")
    val serverJars = config.strings(":ensime-server-jars")
    val pathSep = System.getProperty("path.separator")
    val classpath = serverJars.mkString(pathSep) + pathSep + compilerJars.mkString(pathSep)
    val java = JDK.thisJDK.home.resolve("bin").resolve("java")
    Seq(java.toString,
        "-classpath", classpath,
        "-Dlsp.workspace=" + root,
        // "-Dlsp.logLevel=" + logLevel,
        "org.ensime.server.Server", "--lsp")
  }

  // this Iulian Dragos's LSP client which grew to incorporate Ensime as its backend...
  // worse is better?
  def dragosServerCmd (metaSvc :MetaService, root :Path) = {
    val pkgSvc = metaSvc.service[PackageService]
    val pkgSource = "git:https://github.com/scaled/ensime-project.git"
    val pkgCP = pkgSvc.classpath(pkgSource)
    val langMain = "org.github.dragos.vscode.Main"

    // find a Java 8 VM, as Dragos's client doesn't work with anything newer
    val projJdk = JDK.thisJDK // JDK.jdks.find(_.majorVersion.equals("8")) getOrElse JDK.thisJDK
    val fullCP = projJdk.home.resolve("lib").resolve("tools.jar") +: pkgCP
    val java = projJdk.home.resolve("bin").resolve("java").toString
    Seq(java, "-classpath", fullCP.mkString(System.getProperty("path.separator")),
        "-Xmx768M", // TODO: allow heap size customization?
        "-Dvscode.workspace=" + root,
        // '-Dvscode.logLevel=' + logLevel, // TODO: allow log level customization?
        "-Densime.index.no.reverse.lookups=true",
        langMain, "-stdio")
  }

  /** Converts a Pacman `Package` to `.ensime` config data. */
  def packageToConfig (pkg :Package) :SMap = {
    def slist (elems :SeqV[Any]) = SList(elems.map(e => SString(e.toString)))

    def moduleName (mod :Module) :String =
    if (mod.isDefault) mod.pkg.name else s"${mod.pkg.name}-${mod.name}"

    val m2repo = Paths.get(Props.userHome).resolve(".m2").resolve("repository");
    def toMavenJar (rid :RepoId, suff :String = "") :Option[Path] = {
      val groupPath = (m2repo /: rid.groupId.split("\\."))(_ resolve _)
      val path = groupPath.resolve(rid.artifactId).resolve(rid.version)
                          .resolve(s"${rid.artifactId}-${rid.version}${suff}.${rid.kind}")
      if (Files.exists(path)) Some(path) else None
    }

    def projectId (mod :Module) :SMap = SMap(Map(":project" -> SString(moduleName(mod)),
                                                 ":config" -> SString("compile")))

    def addEnsimeMeta (prjs :Map.Builder[Source, SMap], mods :Map.Builder[String, SMap])
                      (mod :Module) :Unit = {
      val deps = mod.depends(Pacman.repo.resolver)
      val jarDeps = mod.depends.filter(_.scope == Depend.Scope.MAIN).map(_.id).collect({
        case rid :RepoId => rid }).toSeq

      val pinfo = Map.builder[String,SExp]()
      pinfo += (":id", projectId(mod))
      pinfo += (":sources", slist(mod.sourceDirs.values.toSeq))
      pinfo += (":targets", slist(Seq(mod.classesDir)))
      pinfo += (":scalac-options", slist(Seq.view(mod.pkg.scopts)))
      pinfo += (":javac-options", slist(Seq.view(mod.pkg.jcopts)))
      if (!deps.moduleDeps.isEmpty) {
        pinfo += (":depends", SList(deps.moduleDeps.map(d => projectId(d.mod)).toSeq))
      }
      if (!jarDeps.isEmpty) {
        pinfo += (":library-jars", slist(jarDeps.flatMap(d => toMavenJar(d))))
        pinfo += (":library-sources", slist(jarDeps.flatMap(d => toMavenJar(d, "-sources"))))
        pinfo += (":library-docs", slist(jarDeps.flatMap(d => toMavenJar(d, "-javadoc"))))
      }
      prjs += (mod.source, SMap(pinfo.build()))

      val minfo = Map.builder[String,SExp]()
      minfo += (":name", SString(moduleName(mod)))
      minfo += (":source-roots", slist(mod.sourceDirs.values.toSeq))
      minfo += (":targets", slist(Seq(mod.classesDir)))
      if (!deps.moduleDeps.isEmpty) {
        minfo += (":depends-on-modules", slist(deps.moduleDeps.map(_.mod).map(moduleName).toSeq))
      }
      if (!jarDeps.isEmpty) {
        minfo += (":compile-deps", slist(jarDeps.flatMap(d => toMavenJar(d))))
        minfo += (":reference-source-roots", slist(jarDeps.flatMap(d => toMavenJar(d, "-sources"))))
      }
      mods += (moduleName(mod), SMap(minfo.build()))

      // now add info for all dependent projects
      deps.moduleDeps.map(_.mod) foreach addEnsimeMeta(prjs, mods)
    }

    val ensimeServerVersion = "2.0.2" // "3.0.0-SNAPSHOT"
    val ensimeServerId = new RepoId("org.ensime", "server_2.12", ensimeServerVersion, "jar")
    val ensimeServerJars = Pacman.repo.mvn.resolve(ensimeServerId).values.toSeq

    // TODO: get scala version from scala-library depend
    val scalaVersion = "2.12.8"
    val scalacId = new RepoId("org.scala-lang", "scala-compiler", scalaVersion, "jar")
    val scalacJars = Pacman.repo.mvn.resolve(scalacId).values.toSeq

    val info = Map.builder[String,SExp]()
    info += (":root-dir", SString(pkg.root.toString))
    info += (":cache-dir", SString(pkg.root.resolve(".ensime_cache").toString))
    info += (":scala-compiler-jars", slist(scalacJars))
    info += (":ensime-server-version", SString(ensimeServerVersion))
    info += (":ensime-server-jars", slist(ensimeServerJars))
    info += (":name", SString(pkg.name))
    // TODO: find JDK depend for project
    info += (":java-home", SString(JDK.thisJDK.home.toString))
    info += (":java-sources", slist(Seq(JDK.thisJDK.home.resolve("lib").resolve("src.zip"))))
    // TODO: java-flags
    // TODO: java-compiler-args
    // TODO: reference-source-roots
    info += (":scala-version", SString(scalaVersion))
    // TODO: compiler-args
    val prjs = Map.builder[Source,SMap]
    val mods = Map.builder[String,SMap]
    pkg.modules foreach addEnsimeMeta(prjs, mods)
    info += (":subprojects", SList(mods.build().values.toSeq))
    info += (":projects", SList(prjs.build().values.toSeq))
    SMap(info.build)
  }

  def main (args :Array[String]) = {
    def printUsage () = {
      println("Usage: Ensime [command] [args...]")
      println("Commands:")
      println("  make-config [scaled project name]")
    }
    def fail (msg :String) = {
      println(msg)
      System.exit(255)
    }
    if (args.isEmpty) printUsage()
    else args match {
      case Array("make-config", pkgName) => Option.from(Pacman.repo.packageByName(pkgName)) match {
        case None =>
          fail(s"Unable to find '$pkgName' package.")
        case Some(pkg) =>
          val out = new PrintWriter(System.out)
          packageToConfig(pkg).print("", out)
          out.println()
          out.flush()
      }
      case _ => printUsage()
    }
  }
}

class EnsimeLangClient (msvc :MetaService, root :Path)
    extends LangClient(msvc, root, Ensime.ensimeServerCmd(msvc, root)) {

  override def name = "Ensime"
}

// this isn't used but is here as a reference to the Ensime project model;
// as cribbed from the Maven .ensime generator
object EnsimeModel {

  case class Config (
    rootDir :Path,
    cacheDir :Path,
    scalaCompilerJars :Seq[Path],
    ensimeServerJars :Seq[Path],
    ensimeServerVersion :String,
    name :String,
    javaHome :Path,
    javaFlags :Seq[String],
    referenceSourceRoots :Seq[String],
    javaCompilerArgs :Seq[String],
    scalaVersion :String,
    compilerArgs :Seq[String],
    subprojects :Seq[Module],
    projects :Seq[Project])

  case class Module (
    name :String,
    sourceRoots :Seq[Path],
    testRoots :Seq[Path],
    targets :Seq[Path],
    testTargets :Seq[Path],
    dependsOnModules :Seq[String],
    compileDeps :Seq[Path],
    runtimeDeps :Seq[Path],
    testDeps :Seq[Path],
    referenceSourceRoots :Seq[Path],
    docJars :Seq[Path])

  case class ProjectId (name :String, config :String)

  case class Project (
    id :ProjectId,
    depends :Seq[ProjectId],
    sources :Seq[Path],
    targets :Seq[Path],
    scalacOptions :Seq[String],
    javacOptions :Seq[String],
    libraryJars :Seq[Path],
    librarySources :Seq[Path],
    libraryDocs :Seq[Path])
}
