#!/usr/bin/env perl
use File::Basename qw(dirname basename);
require("$ENV{HOME}/m/launcher/Launcher.pl.scala");

run(CLASS => 'Cpan2Nix',
    SCALA => [ '2.12', '-Ywarn-unused-import' ],
    TEE   => "cpan2nix.txt");
=cut
!#

// TODO: remove (=null) modules which are included in perl

import `io.circe::circe-parser:0.9.1`
import `org.yaml:snakeyaml:1.20`

import java.io.File
import java.nio.file.{Paths, Files}
import scala.sys.process._
import scala.util.{Try, Failure, Success}
import scala.collection.JavaConverters._



class Version(s: String) extends Ordered[Version] {
//require(s exists (c => '0'<=c && c<='9'), s"Version `$s' is expected to contain at least a digit")
  private lazy val vectorized: Array[String] = s stripPrefix "v" split "\\s+|(?=[0-9])(?<=[^0-9])|(?=[^0-9])(?<=[0-9])"
  override def hashCode                = vectorized.foldLeft(0)(_ * 31 + _.hashCode)
  override def equals  (that: Any)     = that match { case that1: Version => compare(that1) == 0 /*case _ => false*/ }
  override def compare (that: Version) = Version.compare(this, that)
  override def toString                = s
}
object Version extends Ordering[Version] {
  def apply(s: String) = new Version(s)

  private[this] val INT = "([0-9]+)".r
  def compare(a: Version, b: Version) = {
    val l = a.vectorized.length min b.vectorized.length
    (0 until l).prefixLength(i => a.vectorized(i) equals b.vectorized(i)) match {
      case `l` => a.vectorized.length compare b.vectorized.length
      case i   => a.vectorized(i) compare b.vectorized(i)
    }
  }
  // perl versions usually do not follow semantic version rules, for example:
  require(Version("1.4201") < Version("1.45"))
  require(Version("1.07") < Version("1.3")) // perl specific
}


class Author(s: String) {
  private val upcased: String = s.toUpperCase
  override def hashCode                = upcased.hashCode
  override def equals  (that: Any)     = that match { case that1: Author => upcased == that1.upcased }
  override def toString                = s
}
object Author {
  def apply(s: String) = new Author(s)
}


class Name(s: String) {
  private val upcased: String = s.toUpperCase
  override def hashCode                = upcased.hashCode
  override def equals  (that: Any)     = that match { case that1: Name => upcased == that1.upcased }
  override def toString                = s
}
object Name {
  def apply(s: String) = new Name(s)
}


class Mod(s: String) {
  private val upcased: String = s.toUpperCase
  override def hashCode                = upcased.hashCode
  override def equals  (that: Any)     = that match { case that1: Mod => upcased == that1.upcased }
  override def toString                = s
}
object Mod {
  def apply(s: String) = new Mod(s)
}


class License(s: String) {
  override def hashCode                = s.hashCode
  override def equals  (that: Any)     = that match { case that1: License => s.toString == that1.toString }
  override def toString                = s
}
object License {
  def fromString(s: String): Set[License] = s match {
    case null
       | "unknown"                                                                      => Set.empty
    case "perl_5"       | "http://dev.perl.org/licenses/"
       | "perl"         | "http://opensource.org/licenses/Perl"                         => Set(new License("artistic1"), new License("gpl1Plus"))
    case "open_source"                                                                  => Set(new License("free"))
    case "gpl_1"        | "http://opensource.org/licenses/gpl-license.php"
       | "gpl"          | "http://www.gnu.org/licenses/gpl.html"                        => Set(new License("gpl1Plus"))
    case "gpl_2"        | "http://opensource.org/licenses/gpl-2.0.php"
       | "gpl2"         | "http://www.gnu.org/licenses/old-licenses/gpl-2.0.txt"        => Set(new License("gpl2Plus"))
    case "lgpl_2_1"     | "http://www.gnu.org/licenses/lgpl-2.1.html"
       | "lgpl"         | "http://www.gnu.org/licenses/old-licenses/lgpl-2.1.txt"       => Set(new License("lgpl21Plus"))
    case "lgpl_3_0"                                                                     => Set(new License("lgpl3Plus"))
    case "gpl_3"        | "http://www.gnu.org/licenses/gpl-3.0.txt"                     => Set(new License("gpl3Plus"))
    case "mozilla"      | "http://opensource.org/licenses/mozilla1.1.php"               => Set(new License("mpl11"))
    case "apache_2_0"                                                                   => Set(new License("asl20"))
    case "artistic_1"   | "http://opensource.org/licenses/artistic-license.php"
       | "artistic"                                                                     => Set(new License("artistic1"))
    case "artistic_2"   | "http://www.opensource.org/licenses/artistic-license-2.0.php"
                        | "http://www.opensource.org/licenses/artistic-license-2.0"
                        | "http://opensource.org/licenses/Artistic-2.0"
                        | "http://www.perlfoundation.org/artistic_license_2_0"          => Set(new License("artistic2"))
    case "mit"          | "http://www.opensource.org/licenses/mit-license.php"
                        | "http://opensource.org/licenses/mit-license.php"              => Set(new License("mit"))
    case                  "https://wiki.creativecommons.org/wiki/Public_domain"         => Set(new License("cc0"))
    case "unrestricted"                                                                 => Set(new License("wtfpl"))
    case "bsd"          | "http://opensource.org/licenses/bsd-license.php"              => Set(new License("bsd3"))
    case x                                                                              => println(s"unknown license `$x'"); sys.exit(1)
                                                                                           Set.empty
  }
}


// keep in both forms to minimise produced patches
case class SHA256(base16: String, base32: String)

object SHA256 {
  val BASE16CHARS = "0123456789abcdef"
  val BASE32CHARS = "0123456789abcdfghijklmnpqrsvwxyz"
  def base32ToBase16(s: String): String = {
    require(s.length == 52 && s.forall(BASE32CHARS contains _), s"wrong base32: `$s'")
    val bi = s.map(BASE32CHARS indexOf _).foldLeft(BigInt(0))(_ * 32 + _)
    bi.toByteArray.reverse.padTo(32, 0.toByte).take(32).map("%02x" format _).mkString
  }
  def base16ToBase32(s: String): String = {
    require(s.length == 64 && s.forall(BASE16CHARS contains _), s"wrong base16: `$s'")
    val arr: Array[Byte] = s.map(BASE16CHARS indexOf _).sliding(2,2).map{ case IndexedSeq(h,l) => (h*16+l).toByte }.toArray
    var bi = BigInt(1, arr.reverse)
    var l = List.empty[Char]
    for (i <- 0 until 52) {
      l ::= BASE32CHARS(bi.toInt & 31)
      bi >>= 5
    }
    l.mkString
  }
  def fromString(s: String) = s.length match {
    case 52 => SHA256(base32ToBase16(s), s)
    case 64 => SHA256(s, base16ToBase32(s))
  }
  require(fromString("51cc86edac15c85fe4229e1c515951eefc8b366666122e76077bb1c68e3b27fc") == fromString("1z177f7cdcbv0xv2w4k6cqv8pz7fa5cm274y4bj5zj0mmknqdk2i"))
  require(fromString("3aa4ac1b042b3880438165fb2b2139d377564a8e9928ffe689ede5304ee90558") == fromString("0n05x5731rgdi7kgya4rir55cxyk74hjpyv5h51q0f1b0hdsr91s"))
}




case class NixPackage(maybeauthor: Option[Author], name: Name, version: Version, url: String)

object NixPackage {
  val mirrors    = List( "mirror://cpan"
                       , "http://www.cpan.org"
                       , "http://search.cpan.org/CPAN"
                       , "https://cpan.metacpan.org"
                       , "http://ftp.gwdg.de/pub/languages/perl/CPAN"
                       ).map(_.replace(".", "\\.")).mkString("(?:", "|", ")")
  val extentions = List( ".pm.gz"
                       , ".tar.gz"
                       , ".tar.bz2"
                       , ".tgz"
                       , ".zip"
                       ).map(_.replace(".", "\\.")).mkString("(?:", "|", ")")
  private[this] val re2 = (mirrors + """/+authors/id/[A-Z0-9-]/[A-Z0-9-]{2}/([A-Z0-9-]+)(?:/[^/]+)*/+([^/]+)""" + extentions).r
  private[this] val re3 = (mirrors + """/+modules/by-module/[^/]+/([^/]+)"""                                    + extentions).r
  def fromString(url: String) = url match {
    case re2(a, nameVersion) => val (n, v) = CpanErrata parseNameVersion nameVersion; NixPackage(Some(Author(a)), n, v, url)
    case re3(   nameVersion) => val (n, v) = CpanErrata parseNameVersion nameVersion; NixPackage(None           , n, v, url)
  }
}


class NixPkgs(repopath: String /*File or URL*/) {
  lazy val allPackages: List[NixPackage] = {

    val nixcode = """|let
                     |  pkgs = import <nixpkgs> { config.allowBroken = true; };
                     |  lib = pkgs.lib;
                     |in
                     |  lib.concatMap (x: x.src.urls or []) (lib.attrValues pkgs.perlPackages)
                     |""".stripMargin

    val output = Process("nix-instantiate" :: "--show-trace" :: "--strict" :: "--eval" :: "-E" :: nixcode :: Nil,
                         cwd = None,
                         "NIXPKGS_CONFIG" -> "",
                         "NIX_PATH"       -> s"nixpkgs=${repopath}"
                         ).!!

    "\"([^ \"]+)\"".r.findAllMatchIn(output).map(_ group 1).toList.distinct flatMap { url =>
      Try(NixPackage.fromString(url)) match {
        case Failure(_)  => //System.err.println(s"ignore non-CPAN url $url")
                            None
        case Success(ca) => Some(ca)
      }
    }
  }
}




case class CpanPackage(author: Author, name: Name, version: Version, path: String) {
  lazy val tarballFile: File         = Cpan.downloadFile(this.path) getOrElse (throw new RuntimeException(s"${this.path} not found"))
  lazy val metaFile:    Option[File] = Cpan.downloadFile((NixPackage.extentions+"$").r.replaceAllIn(this.path, ".meta"  ))
//lazy val readmeFile:  Option[File] = Cpan.downloadFile((NixPackage.extentions+"$").r.replaceAllIn(this.path, ".readme"))
  lazy val sha256:      SHA256       = SHA256 fromString s"nix hash-file --type sha256 --base32 ${tarballFile}".!!.trim

  lazy val filesInTarball: Array[String] = if (tarballFile.getName endsWith ".zip")
                                             Array.empty // FIXME: like files in .zip
                                           else
                                             s"tar --list --warning=no-unknown-keyword --file $tarballFile".!! split '\n'
  lazy val isModule:       Boolean       = !name.toString.equalsIgnoreCase("Module-Build") && filesInTarball.contains(s"$name-$version/Build.PL")

  case class MetaExcerpt(runtimeMODs: Map[Mod, Version],
                         buildMODs:   Map[Mod, Version],
                         description: Option[String],
                         licenses:    Set[License],
                         homepage:    Option[String])

  lazy val meta: MetaExcerpt = {
    val metaContent = metaFile.fold("")(file => scala.io.Source.fromFile(file).mkString)
    var runtime     =                                                Map.empty[String,Any]
    var build       = if (isModule) Map("Module::Build" -> "0") else Map.empty[String,Any]
    var description = Option.empty[String]
    var licenses    = Set.empty[License]
    var homepage    = Option.empty[String]

    if (metaContent startsWith "{") {
      val Right(json) = io.circe.parser.parse(metaContent)
      for (path   <- List( List("prereqs", "runtime", "requires") );
           m      <- path.foldLeft[io.circe.ACursor](json.hcursor)(_ downField _).as[Map[String,io.circe.Json]];
           (k, v) <- m) {
        runtime += k -> (v.asString orElse v.asNumber.map(_.toString) getOrElse ???)
      }
      for (path   <- List( List( "prereqs", "configure", "requires")
                         , List( "prereqs", "build",     "requires")
                         , List( "prereqs", "test",      "requires")
                         , List( "prereqs", "test",      "suggests") );
           m      <- path.foldLeft[io.circe.ACursor](json.hcursor)(_ downField _).as[Map[String,io.circe.Json]];
           (k, v) <- m) {
        build += k -> (v.asString orElse v.asNumber.map(_.toString) getOrElse ???)
      }
      description = json.hcursor.downField("abstract").as[String].toOption
      licenses    = json.hcursor.downField("resources").downField("license").as[Set[String]].getOrElse(Set.empty).flatMap(License.fromString(_)) ++
                    json.hcursor.downField(                       "license").as[Set[String]].getOrElse(Set.empty).flatMap(License.fromString(_))
      homepage    = json.hcursor.downField("resources").downField("homepage").as[String].toOption
    } else if (metaContent.nonEmpty) {
      val fixedmeta = metaContent.replace("author:       author:", "author:") // invalid yaml in "String-CamelCase-0.03.meta"
      val yaml: java.util.Map[String, Any] = new org.yaml.snakeyaml.Yaml load fixedmeta
      require(yaml != null, s"invalid yaml $path")

      yaml.get("requires"          ).asInstanceOf[java.util.Map[String,Any]] match { case null => ; case m => runtime ++= m.asScala.mapValues{ case null => "" case v => v } }
      yaml.get("configure_requires").asInstanceOf[java.util.Map[String,Any]] match { case null => ; case m => build   ++= m.asScala.mapValues{ case null => "" case v => v } }
      yaml.get("build_requires"    ).asInstanceOf[java.util.Map[String,Any]] match { case null => ; case m => build   ++= m.asScala.mapValues{ case null => "" case v => v } }
      yaml.get("x_test_requires"   ).asInstanceOf[java.util.Map[String,Any]] match { case null => ; case m => build   ++= m.asScala.mapValues{ case null => "" case v => v } }

      description = Option(yaml.get("abstract").asInstanceOf[String])
      licenses    =  ( Option(yaml.get("license").asInstanceOf[String]).toSet ++
                       ( for (a <- Option(yaml.get("resources").asInstanceOf[java.util.Map[String, String]]);
                              b <- Option(a.get("license")))
                          yield b)
                     ) flatMap (_ split ",\\s*") flatMap (License fromString _)
      homepage    = for (a <- Option(yaml.get("resources").asInstanceOf[java.util.Map[String, String]]);
                         b <- Option(a.get("homepage")))
                    yield b
    }

    // do not include Test::* to propagatedBuildInputs, it might result in colliding the conflicting test libraries (e.g. Test::Simple-13 when an olde version is expected)
    build   ++= runtime.filter   (_._1 startsWith "Test::")
    runtime   = runtime.filterNot(_._1 startsWith "Test::")
    MetaExcerpt( runtime  map { case (m,v) => Mod(m) -> Version(v.toString) }
               , build    map { case (m,v) => Mod(m) -> Version(v.toString) }
               , description filterNot (_ == "unknown")
               , licenses
               , homepage
               )
  }

  def suggestedNixpkgsName: String = name.toString.replace("-", "").replace("_", "")
}


object CpanPackage {
  private[this] val re = ("""[A-Z]/[A-Z]{2}/([A-Z0-9-]+)(?:/[^/]+)*/([^/]+)""" + NixPackage.extentions).r
  def fromPath(path: String) = path match {
    case re(author, nameVersion) => val (name, version) = CpanErrata parseNameVersion nameVersion
                                    CpanPackage(Author(author), name, version, path)
  }
}


object CpanErrata {
  // *** difficult parse cases?
  private[this] val exception1  = "(PerlMagick)-([0-9.]+-[0-9.]+)".r
  private[this] val suspicious1 = "([^/]+)-[0-9.]+-([0-9.]+)".r
  private[this] val rule1       = "([^/]+)-([^/-]+)".r
  private[this] val rule2       = "([^/-]+)_([^_/-]+)".r
  private[this] val rule3       = "([^_/-]+)\\.([^\\._/-]+)".r
  private[this] val rule4       = "([^\\._/-]+)".r
//private[this] val rule3       = "([^_/-]+)".r
  private[this] val nvcache = collection.mutable.Map.empty[String, (Name, Version)] // to avoid flooding same warnings
  def parseNameVersion(s: String): (Name, Version) = nvcache.getOrElseUpdate(s, s match {
    case "Data-Dumper-Lazy"                 => (Name(s),                                 Version(""))
    case "Spreadsheet-ParseExcel-Assist"    => (Name(s),                                 Version(""))
    case "Spreadsheet-WriteExcel-WebPivot2" => (Name("Spreadsheet-WriteExcel-WebPivot"), Version("2"))
    case exception1 (n, v)                  => (Name(n),                                 Version(v))
    case suspicious1(n, v)                  => System.err.println(s"version of `$s` might be detected incorrectly as `$v`")
                                               (Name(n),                                 Version(v))
    case rule1      (n, v)                  => (Name(n),                                 Version(v))
    case rule2      (n, v)                  => (Name(n),                                 Version(v))
    case rule3      (n, v)                  => (Name(n),                                 Version(v))
    case rule4      (n)                     => (Name(n),                                 Version(""))
  })

  // *** modules and packages to ignore
  val modsToIgnore             = Map( Mod("Catalyst::Engine::CGI")                       -> ((_:Version) => true)  // <- these are for old Catalyst-Runtime-5.8xxxx
                                    , Mod("Catalyst::Engine::FastCGI")                   -> ((_:Version) => true)
                                    , Mod("Catalyst::Engine::HTTP")                      -> ((_:Version) => true)
                                    , Mod("Catalyst::Engine::HTTP::Restarter")           -> ((_:Version) => true)
                                    , Mod("Catalyst::Engine::HTTP::Restarter::Watcher")  -> ((_:Version) => true)
                                    )
  val namesToIgnore            = Map( Name("Sys-Virt")                                   -> ((_:Version) => true) // must be updated in-sync with `pkgs.libvirt'
                                    , Name("PathTools")                                  -> ((_:Version) => true) // breaks the installPhase
                                    , Name("Mac-SystemDirectory")                        -> ((_:Version) => true) // fails on linux, I cannot test
                                    , Name("Mac-Pasteboard")                             -> ((_:Version) => true) // fails on linux, I cannot test
                                    , Name("Regexp-Copy")                                -> ((_:Version) => true) // broken
                                    , Name("Socket6")                                    -> ((_:Version) => true) // C code broken "Socket6.xs:109:22: error: 'sv_undef' undeclared (first use in this function); did you mean 'av_undef'?  "
                                    , Name("IO-Socket-INET6")                            -> ((_:Version) => true) // C code broken "Socket6.xs:109:22: error: 'sv_undef' undeclared (first use in this function); did you mean 'av_undef'?  "
                                    , Name("Net-Patricia")                               -> ((_:Version) => true) // C code broken "Socket6.xs:109:22: error: 'sv_undef' undeclared (first use in this function); did you mean 'av_undef'?  "
                                    , Name("GSSAPI")                                     -> ((_:Version) => true) // `heimdal' is broken
                                    , Name("SOAP-Lite")                                  -> ((_:Version) => true) // failed to produce output path '/nix/store/w7kpfrg86sf2ynzv5jvhz1w879pl3igq-perl-IO-SessionData-1.03-devdoc'
                                    , Name("Catalyst-Engine-HTTP-Prefork")               -> ((_:Version) => true) // meta.broken = true
                                    , Name("Catalyst-Plugin-HTML-Widget")                -> ((_:Version) => true) // meta.broken = true
                                    , Name("Devel-SizeMe")                               -> ((_:Version) => true) // meta.broken = true
                                    , Name("Unicode-ICU-Collator")                       -> ((_:Version) => true) // meta.broken = true
                                    , Name("Catalyst-Plugin-Unicode-Encoding")           -> ((_:Version) => true) // now Catalyst-Runtime, to be removed from nixpkgs
                                    , Name("File-DesktopEntry")                          -> ((_:Version) => true) // nixpkgs has configurePhase incompatible with newer versions
                                    , Name("Mail-SPF")                                   -> ((_:Version) => true) // installPhase fails with "ERROR: Can't create '/usr/sbin'"
                                    , Name("GoferTransport-http")                        -> ((_:Version) => true) // installPhase fails with "No rule to make target 'pure_install'"
                                    )

  // *** hack to work with packages wich are out of perl-packages.nix
  val inExternalNixFiles       = Set( Name("Compress-Raw-Zlib")                       // in an external file (todo? move into perl-packages.nix)
                                    , Name("DBD-SQLite")
                                    , Name("DB_File")
                                    )

  // *** do not add to nixpkgs dependencies present on cpan
  val dependenciesToBreak      = Map( Name("ack"                              ) -> Set( Name("Test-Harness"))               // complex buildInputs expression, need to update manually
                                    , Name("File-ShareDir"                    ) -> Set( Name("File-ShareDir-Install"))      // circular dependency
                                    , Name("MooX-Options"                     ) -> Set( Name("MooX-ConfigFromFile"))        // circular dependency
                                    , Name("Plack"                            ) -> Set( Name("CGI-Compile"))                // to disable failing test
                                    , Name("Test-CleanNamespaces"             ) -> Set( Name("Moose")
                                                                                      , Name("MooseX-Role-Parameterized"))  // circular dependency
                                    , Name("Tie-Hash-Indexed"                 ) -> Set( Name("Test"))                       // wrong test framework?
                                    ) withDefaultValue Set.empty


  // *** add to nixpkgs dependencies missing on cpan (usually due to missing .meta file; FIXME: look into Makefile.PL then)
  val extraBuildDependencies   = Map( Name("Autodia"                                 ) -> Map( Mod("DBI")                          -> Version("0"))
                                    , Name("Array-FIFO"                              ) -> Map( Mod("Test::Trap")                   -> Version("0"),
                                                                                               Mod("Test::Deep::NoTest")           -> Version("0"))
                                    , Name("Catalyst-Controller-POD"                 ) -> Map( Mod("inc::Module::Install")         -> Version("0"))
                                    , Name("Catalyst-Runtime"                        ) -> Map( Mod("Type::Tiny")                   -> Version("0"))
                                    , Name("Catalyst-Authentication-Store-DBIx-Class") -> Map( Mod("Test::Warn")                   -> Version("0"))
                                    , Name("Catalyst-Authentication-Store-Htpasswd"  ) -> Map( Mod("Test::WWW::Mechanize")         -> Version("0")
                                                                                             , Mod("Test::LongString")             -> Version("0"))
                                    , Name("Catalyst-Controller-HTML-FormFu"         ) -> Map( Mod("Test::LongString")             -> Version("0"))
                                    , Name("Catalyst-Controller-POD"                 ) -> Map( Mod("Test::WWW::Mechanize")         -> Version("0")
                                                                                             , Mod("Test::LongString")             -> Version("0")
                                                                                             , Mod("inc::Module::Install")         -> Version("0"))
                                    , Name("Catalyst-Plugin-Cache-HTTP"              ) -> Map( Mod("Test::WWW::Mechanize")         -> Version("0")
                                                                                             , Mod("Test::LongString")             -> Version("0"))
                                    , Name("Catalyst-View-Download"                  ) -> Map( Mod("Test::WWW::Mechanize")         -> Version("0")
                                                                                             , Mod("Test::LongString")             -> Version("0"))
                                    , Name("Code-TidyAll"                            ) -> Map( Mod("Test::Class")                  -> Version("0")
                                                                                             , Mod("Test::Deep")                   -> Version("0")
                                                                                             , Mod("Test::Exception")              -> Version("0")
                                                                                             , Mod("Test::Most")                   -> Version("0")
                                                                                             , Mod("Test::Warn")                   -> Version("0"))
                                    , Name("Corona"                                  ) -> Map( Mod("Test::SharedFork")             -> Version("0")
                                                                                             , Mod("Test::TCP")                    -> Version("0"))
                                    , Name("CPAN"                                    ) -> Map( Mod("Archive::Zip")                 -> Version("0"))
                                    , Name("Data-FormValidator"                      ) -> Map( Mod("CGI")                          -> Version("0"))
                                    , Name("Data-Page-Pageset"                       ) -> Map( Mod("Class::Accessor")              -> Version("0")
                                                                                             , Mod("Data::Page")                   -> Version("0")
                                                                                             , Mod("Test::Exception")              -> Version("0"))
                                    , Name("Data-Taxi"                               ) -> Map( Mod("Debug::ShowStuff")             -> Version("0"))
                                    , Name("DateTime-Calendar-Julian"                ) -> Map( Mod("DateTime")                     -> Version("0"))
                                    , Name("DBIx-Introspector"                       ) -> Map( Mod("Test::Fatal")                  -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-CheckChangeLog"        ) -> Map( Mod("Test::Deep")                   -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-ReadmeAnyFromPod"      ) -> Map( Mod("Test::SharedFork")             -> Version("0")
                                                                                             , Mod("Test::Differences")            -> Version("0")
                                                                                             , Mod("Test::Exception")              -> Version("0")
                                                                                             , Mod("Test::Warn")                   -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-ReadmeMarkdownFromPod" ) -> Map( Mod("Test::Deep")                   -> Version("0")
                                                                                             , Mod("Test::Differences")            -> Version("0")
                                                                                             , Mod("Test::Exception")              -> Version("0")
                                                                                             , Mod("Test::Warn")                   -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-CPAN-Changes"     ) -> Map( Mod("Test::Deep")                   -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-CPAN-Meta-JSON"   ) -> Map( Mod("Test::Deep")                   -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-DistManifest"     ) -> Map( Mod("Test::Deep")                   -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-MinimumVersion"   ) -> Map( Mod("Test::Deep")                   -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-Perl-Critic"      ) -> Map( Mod("Test::Deep")                   -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-Synopsis"         ) -> Map( Mod("Test::Deep")                   -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-UnusedVars"       ) -> Map( Mod("Test::Deep")                   -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-Version"          ) -> Map( Mod("Test::Deep")                   -> Version("0"))
                                    , Name("Font-TTF"                                ) -> Map( Mod("IO::String")                   -> Version("0"))
                                    , Name("FormValidator-Simple"                    ) -> Map( Mod("CGI")                          -> Version("0"))
                                    , Name("Gnome2-Canvas"                           ) -> Map( Mod("ExtUtils::Depends")            -> Version("0"))
                                    , Name("Gnome2-Canvas"                           ) -> Map( Mod("ExtUtils::PkgConfig")          -> Version("0")
                                                                                             , Mod("ExtUtils::Depends")            -> Version("0"))
                                    , Name("Gtk2-TrayIcon"                           ) -> Map( Mod("ExtUtils::PkgConfig")          -> Version("0")
                                                                                             , Mod("ExtUtils::Depends")            -> Version("0")
                                                                                             , Mod("Glib::CodeGen")                -> Version("0"))
                                    , Name("Gtk2-Unique"                             ) -> Map( Mod("Glib::CodeGen")                -> Version("0"))
                                    , Name("Hash-Merge-Simple"                       ) -> Map( Mod("Test::Deep")                   -> Version("0")
                                                                                             , Mod("Test::Warn")                   -> Version("0")
                                                                                             , Mod("Test::Exception")              -> Version("0")
                                                                                             , Mod("Test::Differences")            -> Version("0"))
                                    , Name("HTML-Selector-XPath"                     ) -> Map( Mod("Test::Base")                   -> Version("0"))
                                    , Name("HTML-Tidy"                               ) -> Map( Mod("Test::Exception")              -> Version("0"))
                                    , Name("HTTP-Response-Encoding"                  ) -> Map( Mod("LWP::UserAgent")               -> Version("0"))
                                    , Name("IO-Socket-Timeout"                       ) -> Map( Mod("Test::SharedFork")             -> Version("0"))
                                    , Name("Module-Build-Pluggable-PPPort"           ) -> Map( Mod("Test::SharedFork")             -> Version("0"))
                                    , Name("Module-Info"                             ) -> Map( Mod("Test::Pod")                    -> Version("0")
                                                                                             , Mod("Test::Pod::Coverage")          -> Version("0"))
                                    , Name("MooseX-Has-Options"                      ) -> Map( Mod("Test::Deep")                   -> Version("0"),
                                                                                               Mod("Test::Differences")            -> Version("0"),
                                                                                               Mod("Test::Exception")              -> Version("0"),
                                                                                               Mod("Test::Warn")                   -> Version("0"))
                                    , Name("PerlIO-via-symlink"                      ) -> Map( Mod("inc::Module::Install")         -> Version("0"))
                                    , Name("PerlIO-via-Timeout"                      ) -> Map( Mod("Test::SharedFork")             -> Version("0"))
                                    , Name("Plack"                                   ) -> Map( Mod("Test::SharedFork")             -> Version("0"))
                                    , Name("Plack-App-Proxy"                         ) -> Map( Mod("Test::SharedFork")             -> Version("0"))
                                    , Name("Plack-Test-ExternalServer"               ) -> Map( Mod("Test::SharedFork")             -> Version("0"))
                                    , Name("Plack-Middleware-Auth-Digest"            ) -> Map( Mod("Test::SharedFork")             -> Version("0")
                                                                                             , Mod("Test::TCP")                    -> Version("0"))
                                    , Name("Plack-Middleware-Deflater"               ) -> Map( Mod("Test::SharedFork")             -> Version("0")
                                                                                             , Mod("Test::TCP")                    -> Version("0"))
                                    , Name("Plack-Middleware-Session"                ) -> Map( Mod("Test::SharedFork")             -> Version("0")
                                                                                             , Mod("Test::TCP")                    -> Version("0"))
                                    , Name("Protocol-HTTP2"                          ) -> Map( Mod("Test::SharedFork")             -> Version("0"))
                                    , Name("REST-Utils"                              ) -> Map( Mod("Test::LongString")             -> Version("0")
                                                                                             , Mod("Test::WWW::Mechanize")         -> Version("0"))
                                    , Name("RT-Client-REST"                          ) -> Map( Mod("CGI")                          -> Version("0")
                                                                                             , Mod("DateTime")                     -> Version("0")
                                                                                             , Mod("DateTime::Format::DateParse")  -> Version("0")
                                                                                             , Mod("Error")                        -> Version("0")
                                                                                             , Mod("Exception::Class")             -> Version("0")
                                                                                             , Mod("HTTP::Cookies")                -> Version("0")
                                                                                             , Mod("LWP::UserAgent")               -> Version("0")
                                                                                             , Mod("Params::Validate")             -> Version("0")
                                                                                             , Mod("Test::Exception")              -> Version("0"))
                                    , Name("Starlet"                                 ) -> Map( Mod("Test::SharedFork")             -> Version("0"))
                                    , Name("Task-Plack"                              ) -> Map( Mod("Test::SharedFork")             -> Version("0"))
                                    , Name("Task-FreecellSolver-Testing"             ) -> Map( Mod("Test::Trap")                   -> Version("0"))
                                    , Name("Term-ProgressBar-Simple"                 ) -> Map( Mod("Test::MockObject")             -> Version("0"))
                                    , Name("Test-Class-Most"                         ) -> Map( Mod("Test::Differences")            -> Version("0")
                                                                                             , Mod("Test::Deep")                   -> Version("0")
                                                                                             , Mod("Test::Exception")              -> Version("0")
                                                                                             , Mod("Test::Warn")                   -> Version("0"))
                                    , Name("Test-Run-Plugin-ColorFileVerdicts"       ) -> Map( Mod("Test::Trap")                   -> Version("0"))
                                    , Name("Test-Run-Plugin-ColorSummary"            ) -> Map( Mod("Test::Trap")                   -> Version("0"))
                                    , Name("Test-WWW-Mechanize"                      ) -> Map( Mod("Test::LongString")             -> Version("0") )
                                    , Name("Test-WWW-Mechanize-CGI"                  ) -> Map( Mod("Test::LongString")             -> Version("0"))
                                    , Name("Test-WWW-Mechanize-PSGI"                 ) -> Map( Mod("Test::LongString")             -> Version("0"))
                                    , Name("Twiggy"                                  ) -> Map( Mod("Test::SharedFork")             -> Version("0"))
                                    , Name("YAML"                                    ) -> Map( Mod("Test::Base")                   -> Version("0"))
                                    ) withDefaultValue Map.empty
  val extraRuntimeDependencies = Map( Name("Any-Moose"                        ) -> Map( Mod("Mouse")                        -> Version("0")
                                                                                      , Mod("Moose")                        -> Version("0"))
                                    , Name("Crypt-PKCS10"                     ) -> Map( Mod("Convert::ASN1")                -> Version("0"))
                                    , Name("GDTextUtil"                       ) -> Map( Mod("GD")                           -> Version("0"))
                                    , Name("Gtk2-Unique"                      ) -> Map( Mod("Cairo")                        -> Version("0")
                                                                                      , Mod("Pango")                        -> Version("0"))
                                    , Name("Gtk2-ImageView"                   ) -> Map( Mod("Pango")                        -> Version("0"))
                                    , Name("Gtk2-TrayIcon"                    ) -> Map( Mod("Pango")                        -> Version("0"))
                                    , Name("Gtk2-GladeXML"                    ) -> Map( Mod("Pango")                        -> Version("0"))
                                    , Name("Goo-Canvas"                       ) -> Map( Mod("Pango")                        -> Version("0"))
                                    , Name("Gnome2-Wnck"                      ) -> Map( Mod("Pango")                        -> Version("0"))
                                    , Name("Gnome2-Canvas"                    ) -> Map( Mod("Glib")                         -> Version("0")
                                                                                      , Mod("Gtk2")                         -> Version("0")
                                                                                      , Mod("Pango")                        -> Version("0"))
                                    , Name("libxml-perl"                      ) -> Map( Mod("XML::Parser")                  -> Version("0"))
                                    , Name("Linux-Inotify2"                   ) -> Map( Mod("common::sense")                -> Version("0"))
                                    , Name("Log-LogLite"                      ) -> Map( Mod("IO::LockedFile")               -> Version("0"))
                                    , Name("Proc-WaitStat"                    ) -> Map( Mod("IPC::Signal")                  -> Version("0"))
                                    , Name("RSS-Parser-Lite"                  ) -> Map( Mod("local::lib")                   -> Version("0"))
                                    , Name("Statistics-TTest"                 ) -> Map( Mod("Statistics::Distributions")    -> Version("0")
                                                                                      , Mod("Statistics::Descriptive")      -> Version("0"))
                                    , Name("Text-WrapI18N"                    ) -> Map( Mod("Text::CharWidth")              -> Version("0"))
                                    , Name("XML-SAX"                          ) -> Map( Mod("XML::SAX::Exception")          -> Version("0"))
                                    , Name("XML-Grove"                        ) -> Map( Mod("Data::Grove")                  -> Version("0"))
                                    , Name("XML-Handler-YAWriter"             ) -> Map( Mod("XML::Parser::PerlSAX")         -> Version("0"))
                                    ) withDefaultValue Map.empty

  // *** pinned packages
  val pinnedPackages           = Set( CpanPackage fromPath "N/NJ/NJH/MusicBrainz-DiscID-0.03.tar.gz"                 // need to review patchPhase manually
//                                  , CpanPackage fromPath "P/PM/PMQS/Compress-Raw-Zlib-2.074.tar.gz"                // in external .nix file, need to update manually (in perl)
//                                  , CpanPackage fromPath "I/IS/ISHIGAKI/DBD-SQLite-1.56.tar.gz"                    // in external .nix file, need to update manually
//                                  , CpanPackage fromPath "P/PM/PMQS/DB_File-1.831.tar.gz"                          // in external .nix file, need to update manually
                                    , CpanPackage fromPath "L/LD/LDS/GD-2.53.tar.gz"                                 // nixpkgs has .patch file incompatible with newer versions
                                    , CpanPackage fromPath "D/DR/DROLSKY/MooseX-AttributeHelpers-0.23.tar.gz"        // nixpkgs has .patch file incompatible with newer versions
                                    , CpanPackage fromPath "G/GA/GAAS/Unicode-String-2.09.tar.gz"                    // nixpkgs has .patch file incompatible with newer versions
                                    , CpanPackage fromPath "J/JH/JHI/Time-HiRes-1.9753.tar.gz"                       // 1.9753->1.9754 broke tests of Catalyst-Runtime
                                    , CpanPackage fromPath "M/MA/MARKOV/MIME-Types-2.13.tar.gz"                      // 2.13-~->2.17  broke tests of Catalyst
//                                  , CpanPackage fromPath "U/UM/UMEMOTO/Socket6-0.28.tar.gz"                        // 2018-03-08: broken on staging (ok on master)
//                                  , CpanPackage fromPath "A/AG/AGROLMS/GSSAPI-0.28.tar.gz"                         // 2018-03-08: broken on staging (ok on master)
                                    , CpanPackage fromPath "M/MS/MSISK/HTML-TableExtract-2.13.tar.gz"                // 2.15 seems broken
                                    , CpanPackage fromPath "M/MA/MAKAMAKA/JSON-2.90.tar.gz"                          // 2.97xx test failed
                                    , CpanPackage fromPath "R/RU/RURBAN/B-C-1.54.tar.gz"                             // 1.55: No rule to make target 'subdirs-test_dynamic', needed by 'test'
                                    , CpanPackage fromPath "N/NI/NIGELM/Catalyst-Controller-HTML-FormFu-2.02.tar.gz" // 2.04 test failed
                                    , CpanPackage fromPath "R/RR/RRA/podlators-4.10.tar.gz"                          // 4.11 test failed
                                    )

  // *** enforce 'doCheck = false' or 'doCheck = false'
  val doCheckOverride          = Map( Name("PathTools")                            -> false
                                    , Name("Net-HTTP")                             -> false
                                    , Name("Net-Amazon-MechanicalTurk")            -> false // wants network
                                    , Name("NetAddr-IP")                           -> false // 2018-03-08: tests broken on staging (ok on master)
                                    , Name("JSON")                                 -> false // 2018-03-08: tests broken on staging (ok on master)
                                    , Name("YAML-LibYAML")                         -> false // 2018-03-08: tests broken on staging (ok on master)
                                    , Name("Razor2-Client-Agent")                  -> false // 2018-03-08: tests broken on staging (ok on master)
                                    , Name("Compress-Bzip2")                       -> false // 2018-03-08: tests broken on staging (test fails with "syntax error near unexpected token `0,' ")
                                    , Name("Task-Catalyst-Tutorial")               -> false // fails with "open3: exec of .. perl .. failed: Argument list too long at .../TAP/Parser/Iterator/Process.pm line 165."
                                    , Name("Dist-Zilla-PluginBundle-TestingMania") -> false // fails with "open3: exec of .. perl .. failed: Argument list too long at .../TAP/Parser/Iterator/Process.pm line 165."
                                    , Name("RSS-Parser-Lite")                      -> false // creates files in $HOME
                                    , Name("B-C")                                  -> false // test failed, mark it broken?
                                    )
}



object Cpan {
  private val allPackages     = new collection.mutable.HashMap[CpanPackage, CpanPackage]
          val byMod           = new collection.mutable.HashMap[Mod,            Set[CpanPackage]] withDefaultValue Set.empty
          val byName          = new collection.mutable.HashMap[Name,           Set[CpanPackage]] withDefaultValue Set.empty
          val byAuthorAndName = new collection.mutable.HashMap[(Author, Name), Set[CpanPackage]] withDefaultValue Set.empty

  locally {
    if (!new File("02packages.details.txt").exists) {
      "wget https://raw.githubusercontent.com/metacpan/metacpan-cpan-extracted/master/02packages.details.txt".!
    }
    val re = ("""(\S+)\s+\S+\s+([A-Z]/[A-Z]{2}/[A-Z0-9-]+(?:/[^/]+)*/[^/]+""" + NixPackage.extentions + ")").r
    scala.io.Source.fromFile("02packages.details.txt").getLines dropWhile(_.nonEmpty) dropWhile(_.isEmpty) foreach {
      case re(mod, path) => Try(CpanPackage fromPath path) match {
                              case Success(cp) =>
                                val cpinterned = allPackages.getOrElseUpdate(cp, cp)
                                byMod          (Mod(mod)          ) += cpinterned
                                byName         (           cp.name) += cpinterned
                                byAuthorAndName(cp.author->cp.name) += cpinterned
                              case Failure(e) =>
                                System.err.println(s"ignore `$path' $e")
                            }
      case line          => System.err.println(s"ignore `$line'")
    }
  }


  def downloadFile(path: String): Option[File] = {
    val localfilename     = new File(s"./spool/${Paths.get(path).getFileName}")
    val localfilename_tmp = new File(s"./spool/${Paths.get(path).getFileName}.tmp")
    val err404witness     = new File(localfilename.getPath+".err404witness")
    if (localfilename.exists) {
      Some(localfilename)
    } else if (err404witness.exists) {
      None
    } else {
      println(s"downloading $localfilename")

      localfilename_tmp.getParentFile.mkdir()
      val fos = new java.io.FileOutputStream(localfilename_tmp)
      try {
        fos.getChannel.transferFrom(java.nio.channels.Channels.newChannel(new java.net.URL(s"https://cpan.metacpan.org/authors/id/${path}").openStream), 0, Long.MaxValue)
        fos.close()
        localfilename_tmp.renameTo(localfilename)
        Some(localfilename)
      } catch {
        case _: java.io.FileNotFoundException => // remember that there was 404
          fos.close()
          localfilename_tmp.delete()
          val fos2 = new java.io.FileOutputStream(err404witness)
          fos2.write(Array.emptyByteArray)
          fos2.close()
          None
        case e: Throwable =>
          fos.close()
          localfilename_tmp.delete()
          throw e
      }
    }
  }
}


class PullRequester(repopath: File) {
  object LocalPerl {
    var theOldestSupportedPerl = Process( "nix-build" :: "--show-trace"
                                       :: "--option" :: "binary-caches" :: "http://nix-cache.s3.amazonaws.com/"
                                       :: "-E" :: "(import <nixpkgs> { }).perl522" :: Nil,
                                          cwd = repopath,
                                          "NIXPKGS_CONFIG" -> "",
                                          "NIX_PATH"       -> s"nixpkgs=${repopath.getAbsolutePath}"
                                        ).!!.trim

    private[this] val localVersions = collection.mutable.HashMap.empty[Mod, Option[Version]]
    def localVersion(mod: Mod): Option[Version] = localVersions.getOrElseUpdate(mod, {
      val lv = if (mod.toString equalsIgnoreCase "if")  // "perl -e 'print $if::VERSION'" does not work
                 Some(Version("0.0606"))
               else
                 Try(Process( List("bash", "-c", s"$theOldestSupportedPerl/bin/perl -M$mod -e 'print $$$mod::VERSION' 2>/dev/null")
                            , cwd = None
                            , "PERL5LIB" -> s"$theOldestSupportedPerl/lib/perl5/site_perl"
                            ).!!.trim).toOption map (Version(_))
      println(s"local $mod = $lv")
      lv
    })
  }

  // a typical code block in `perl-packages.nix`
  case class BuildPerlPackageBlock(source: String) {
    val nixpkgsName:                        String   =                   """(?s)^  (\S+)"""                    .r.findFirstMatchIn(source).get.group(1)
    val versionString:               Option[String]  =                   """(?s)version\s*=\s*"([^"]+)";"""    .r.findFirstMatchIn(source).map(_ group 1)
    val nameAndVersion:                     String   =                   """(?s)name\s*=\s*"([^"]+)";"""       .r.findFirstMatchIn(source).get.group(1)
    val url:                                String   = /*Try { */        """(?s)url\s*=\s*"?([^";]+)"?;"""     .r.findFirstMatchIn(source).get.group(1) /*} getOrElse {  println(source); ??? }*/
    val sha256:                             SHA256   = SHA256 fromString """(?s)sha256\s*=\s*"([a-z0-9]+)";""" .r.findFirstMatchIn(source).get.group(1)
    val resolvedNameAndVersion:             String   = versionString.fold(nameAndVersion)(nameAndVersion.replace("${version}", _))
    val resolvedUrl:                        String   = versionString.fold(url           )(url           .replace("${version}", _)).replace("${name}", resolvedNameAndVersion)
    val propagatedBuildInputs: Option[Array[String]] = """(?s)propagatedBuildInputs\s*=\s*\[([^]]*)\]"""       .r.findFirstMatchIn(source).map(_ group 1 split "\\s+")
    val buildInputs:           Option[Array[String]] = """(?s)buildInputs\s*=\s*\[([^]]*)\]"""                 .r.findFirstMatchIn(source).map(_ group 1 split "\\s+")
    val licenses:              Option[Array[String]] = """(?s)license\s*=\s*(with[^;]+;\s*)\[([^]]*)\]"""      .r.findFirstMatchIn(source).map(_ group 1 split "\\s+")
    val doCheck:                     Option[String]  =                   """(?s)doCheck\s*=\s*([^;]+);"""      .r.findFirstMatchIn(source).map(_ group 1)

    val (name, version) = CpanErrata.parseNameVersion(resolvedNameAndVersion)

    def copy( builder:               String
            , versionString:         String
            , nameAndVersion:        String
            , url:                   String
            , sha256:                SHA256
            , doCheckOverride:       Option[Boolean]
            , buildInputs:           Traversable[String]
            , propagatedBuildInputs: Traversable[String]
            , licenses:              Traversable[License]
            ): BuildPerlPackageBlock = {
      var s = source
      s = """\bbuildPerl(Package|Module)\b"""   .r.replaceAllIn(s, builder)
      s = """(?s)version\s*=\s*"[^"]+";"""      .r.replaceAllIn(s, s"version = \042${versionString}\042;")

      if (nameAndVersion != new BuildPerlPackageBlock(s).resolvedNameAndVersion)
        s = """(?s)name\s*=\s*"[^"]+";"""         .r.replaceAllIn(s, s"name = \042${nameAndVersion}\042;")

      if (url != new BuildPerlPackageBlock(s).resolvedUrl)
        s = """(?s)url\s*=\s*"?([^";]+)"?;"""     .r.replaceAllIn(s, s"url = ${url};")

      s = """(?s)sha256\s*=\s*"([a-z0-9]+)";""" .r.replaceSomeIn(s, m => m.group(1).length match {
                                                                           case 64 => Some(s"sha256 = \042${sha256.base16}\042;")
                                                                           case 52 => Some(s"sha256 = \042${sha256.base32}\042;")
                                                                         })
      (this.doCheck, doCheckOverride) match {
        case (None,      Some(false)      ) => val a = s.split('\n')
                                               s = (a.init :+ s"    doCheck = false;" :+ a.last) mkString "\n"
        case (`doCheck`, _                ) =>
        case (Some(_),   None | Some(true)) => ??? // FIXME: remove doCheck
        case (Some(_),   Some(false)      ) => s = """(?s) doCheck\s*=\s*[^;]+;""".r.replaceAllIn(s, s" doCheck = false;")
      }

      (this.propagatedBuildInputs, propagatedBuildInputs.nonEmpty) match {
        case (None   , false) =>
        case (None   , true ) => val a = s.split('\n')
                                 s = (a.init :+ s"    propagatedBuildInputs = [ ${propagatedBuildInputs mkString " "} ];" :+ a.last) mkString "\n"
        case (Some(_), false) => s = """(?s)\s+propagatedBuildInputs\s*=\s*([^;]+);""".r.replaceAllIn(s, "")
        case (Some(_), true ) => s = """(?s) propagatedBuildInputs\s*=\s*[^;]+;""".r.replaceAllIn(s, s" propagatedBuildInputs = [ ${propagatedBuildInputs mkString " "} ];")
      }

      (this.buildInputs, buildInputs.nonEmpty) match {
        case (None   , false) =>
        case (None   , true ) => val a = s.split('\n')
                               s = (a.init :+ s"    buildInputs = [ ${buildInputs mkString " "} ];" :+ a.last) mkString "\n"
        case (Some(_), false) => s = """(?s)\s+buildInputs\s*=\s*([^;]+);""".r.replaceAllIn(s, "")
        case (Some(_), true ) => s = """(?s) buildInputs\s*=\s*[^;]+;""".r.replaceAllIn(s, s" buildInputs = [ ${buildInputs mkString " "} ];")
      }

      (this.licenses, licenses.nonEmpty) match {
        case (None   , false) =>
        case (None   , true ) => // ???: insert licenses which were absent
        case (Some(_), false) => // ???: remove licenses which were present
        case (Some(_), true ) => // ???: replace existing license?
                                 //s = """(?s) license\s*=\s*(with[^;]+;\s*)[^;]+;""".r.replaceAllIn(s, s" license = with stdenv.lib.licenses; [ ${licenses mkString " "} ];")
      }

      // FIXME: update homepage
      new BuildPerlPackageBlock(s)
    }

    def updatedTo(cp: CpanPackage): BuildPerlPackageBlock =
      copy( builder               = if (cp.isModule) "buildPerlModule" else "buildPerlPackage"
          , versionString         = cp.version.toString stripPrefix "v"
          , nameAndVersion        = s"${cp.name}-${cp.version.toString stripPrefix "v"}"
          , url                   = s"mirror://cpan/authors/id/${cp.path}"
          , sha256                = cp.sha256
          , doCheckOverride       = CpanErrata.doCheckOverride get cp.name
          , propagatedBuildInputs = propagatedBuildInputs.toList.flatten.filter(_ startsWith "pkgs.") ++ (runtimeDeps(cp) -- runtimeDeps(cp).flatMap(deepRuntimeDeps _)).map(escapedNixifiedName).toArray.sorted
          , buildInputs           =           buildInputs.toList.flatten.filter(_ startsWith "pkgs.") ++ (buildDeps(cp)   -- deepRuntimeDeps(cp)                       ).map(escapedNixifiedName).toArray.sorted
          , licenses              = cp.meta.licenses
          )

    def this(cp: CpanPackage) = this {
      val sb = new java.lang.StringBuilder
      sb append s"""  ${cp.suggestedNixpkgsName} = ${if (cp.isModule) "buildPerlModule" else "buildPerlPackage"} rec {\n"""
      sb append s"""     name = "${cp.name}-${cp.version}";\n"""
      sb append s"""     src = fetchurl {\n"""
      sb append s"""       url = mirror://cpan/authors/id/${cp.path};\n"""
      sb append s"""       sha256 = "${cp.sha256.base32}";\n"""
      sb append s"""     };\n"""
      (runtimeDeps(cp) -- runtimeDeps(cp).flatMap(deepRuntimeDeps _)).toArray match {
        case Array() =>
        case a       => sb append s"""     propagatedBuildInputs = [ ${a.map(escapedNixifiedName).sorted mkString " "} ];\n"""
      }
      (buildDeps(cp)   -- deepRuntimeDeps(cp)                       ).toArray match {
        case Array() =>
        case a       => sb append s"""     buildInputs = [ ${a.map(escapedNixifiedName).sorted mkString " "} ];\n"""
      }
      CpanErrata.doCheckOverride.get(cp.name) match {
        case Some(false)       => sb append s"""       doCheck = false;\n"""
        case Some(true) | None =>
      }
      sb append s"""     meta = {\n"""
      cp.meta.description foreach { text =>
        sb append s"""       description = "${text}";\n"""
      }
      if (cp.meta.licenses.nonEmpty) {
        sb append s"""       license = with stdenv.lib.licenses; [ ${cp.meta.licenses mkString " "} ];\n"""
      }
      if (cp.meta.homepage.nonEmpty) {
        sb append s"""       homepage = "${cp.meta.homepage.get}";\n"""
      }
      sb append s"""     };\n"""
      sb append s"""  };\n"""
      sb.toString
    }
  }


  var `perl-packages.nix` = scala.io.Source.fromFile(new File(repopath, "/pkgs/top-level/perl-packages.nix")).mkString
  var buildPerlPackageBlocks = collection.immutable.TreeMap.empty[String, BuildPerlPackageBlock]
  for (source <- """(?s)  \S+\s*=\s*(let .+? in\s*)?buildPerl(Package|Module) (rec )?\{.+?\n {1,3}\};\n""".r.findAllIn(`perl-packages.nix`);
       bppb <- Try(new BuildPerlPackageBlock(source)) /*match { case Success(b) => Success(b); case Failure(e) => println(source); e.printStackTrace(); Failure(e) }*/ ) {
    buildPerlPackageBlocks += bppb.nixpkgsName -> bppb
  }

  def nixifiedName(cp: CpanPackage) = buildPerlPackageBlocks.filter(_._2.name == cp.name).toList match {
    case Nil                                              => cp.suggestedNixpkgsName
    case (nixpkgsName, bppb)::Nil                         => nixpkgsName
    case blocks if cp.name==Name("Archive-Zip")           => "ArchiveZip"
    case blocks                                           => throw new RuntimeException(s"$cp can be one of ${blocks map (_._1)}")
  }

  def escapedNixifiedName(cp: CpanPackage) = nixifiedName(cp) match {
    case "version" => "self.version"
    case "if"      => "self.\"if\""
    case s         => s
  }


  private def modToPackage(mod: Mod, version: Version): Option[CpanPackage] =
    if (mod.toString equalsIgnoreCase "perl")
      None
    else
      LocalPerl.localVersion(mod) match {
        case Some(localver) if version<=localver                      => None
        case _ if CpanErrata.modsToIgnore.get(mod).exists(_(version)) => None
        case _                                                        =>
          Cpan.byMod(mod).toList match {
            case Nil                                                                    => throw new RuntimeException(s"mod `$mod' not found, maybe ${Cpan.byMod.keys filter (_.toString.toUpperCase.replaceAll("[:-]","") == mod.toString.toUpperCase.replaceAll("[:-]","")) mkString " "}");
            case cp::Nil if cp.name.toString equalsIgnoreCase "perl"                    => None
            case cp::Nil if CpanErrata.namesToIgnore.get(cp.name).exists(_(cp.version)) => //println(s"package ${cp} ignored")
                                                                                           None
            case cp::Nil                                                                => CpanErrata.pinnedPackages.find(_.name == cp.name) match {
                                                                                             case Some(pinnedcp) => Some(pinnedcp)
                                                                                             case None           => Some(cp)
                                                                                           }
            case cpps                                                                   => throw new RuntimeException(s"mod `$mod' provided by many $cpps");
          }
      }

  private def filterDeps(cp: CpanPackage, deps: Iterable[CpanPackage]) = for (d <- deps if d != cp;
                                                                                        if !(d.name.toString.equalsIgnoreCase("Module-Build") && cp.isModule);
                                                                                        if !(CpanErrata.dependenciesToBreak(cp.name) contains d.name))
                                                                         yield d

  private def buildDeps      (cp: CpanPackage): Set[CpanPackage] = filterDeps(cp, cp.meta.buildMODs   ++ CpanErrata.extraBuildDependencies  (cp.name) flatMap { case (m,v) => modToPackage(m,v) }).toSet
  private def runtimeDeps    (cp: CpanPackage): Set[CpanPackage] = filterDeps(cp, cp.meta.runtimeMODs ++ CpanErrata.extraRuntimeDependencies(cp.name) flatMap { case (m,v) => modToPackage(m,v) }).toSet
  private def deepRuntimeDeps(cp: CpanPackage): Set[CpanPackage] = runtimeDeps(cp) flatMap (d => deepRuntimeDeps(d) + d)

  private val _allDeps = collection.mutable.HashMap.empty[CpanPackage, Set[CpanPackage]]
  def allDeps(cp: CpanPackage, seen: List[CpanPackage]=Nil): Set[CpanPackage] = _allDeps.getOrElseUpdate(cp, {
    if (seen contains cp) {
      println(s"circular dependency ${(cp::seen.takeWhile(_ != cp):::cp::Nil).reverse map (_.name) mkString " -> "}")
      Set.empty
    } else {
      runtimeDeps(cp)++buildDeps(cp) flatMap (d => allDeps(d, cp :: seen) + d)
    }
  })



  def prepareCommit(np: NixPackage, cp: CpanPackage): Option[String] = {
    allDeps(cp) // to fail earlier if circular deps found

/*
    // debug print
    println(s"  prepareCommit($np, $cp)")
    println(allDeps(cp) mkString "\n")
    println(s"  meta.buildMODs:")
    println(cp.meta.buildMODs mkString "\n")
    println(s"  buildDeps:")
    println(buildDeps(cp) mkString "\n")

    println(s"  meta.runtimeMODs:")
    println(cp.meta.runtimeMODs mkString "\n")
    println(s"  runtimeDeps:")
    println(runtimeDeps(cp) mkString "\n")
*/

    var message = List.empty[String]

    buildPerlPackageBlocks find (_._2.resolvedUrl == np.url) match {
      case None =>
        System.err.println(s"$np->$cp not found in perl-packages.nix")
        None
      case Some((_, block)) =>
        val newBlock = block.updatedTo(cp)

        if (np.version != cp.version)
          message ::= s"perlPackages.${block.nixpkgsName}: ${np.version} -> ${cp.version}"
        else
          message ::= s"perlPackages.${block.nixpkgsName}: cleanup"


        val depBlocks: Set[Either[(BuildPerlPackageBlock, CpanPackage), BuildPerlPackageBlock]] =
          allDeps(cp) flatMap { dep =>
            buildPerlPackageBlocks.filter(_._2.name == dep.name).toList match {
              case Nil        => Right(new BuildPerlPackageBlock(dep)) :: Nil // Try(new BuildPerlPackageBlock(dep)).toOption.map(Right(_))
              case depblocks  => //if (depblocks.length>1) println("depblocks:"::depblocks mkString "\n")
                                 depblocks map {case (_, bppb) => Left(bppb->dep)}
            }
          }

/*
        // debug print
        println(s"======================== ${np.name}: ${np.version} -> ${cp.version}")
        println(block   .source split '\n' map ("< "+_) mkString "\n")
        println(newBlock.source split '\n' map ("> "+_) mkString "\n")
        depBlocks foreach {
          case Left((bppb, dep)) => // upgrade/cleanup existing dep
            val newBppb = bppb.updatedTo(dep)
            if (bppb.source.trim != newBppb.source.trim) {
              println(bppb   .source split '\n' map ("< "+_) mkString "\n")
              println(newBppb.source split '\n' map ("> "+_) mkString "\n")
            }
          case Right(newBppb)       => // insert new dep
            println(newBppb.source split '\n' map ("+ "+_) mkString "\n")
        }
*/

        // do mutate `perl-packages.nix`
        buildPerlPackageBlocks = buildPerlPackageBlocks - block.nixpkgsName + (newBlock.nixpkgsName -> newBlock)
        `perl-packages.nix` = `perl-packages.nix`.replace(block.source.trim, newBlock.source.trim)

        depBlocks foreach {
          case Left((bppb, dep)) => // upgrade/cleanup existing dep
            val newBppb = bppb.updatedTo(dep)
            if (bppb.source.trim != newBppb.source.trim) {
              if (bppb.version != newBppb.version)
                message ::= s"perlPackages.${bppb.nixpkgsName}: ${bppb.version} -> ${newBppb.version}"
              else
                message ::= s"perlPackages.${bppb.nixpkgsName}: cleanup"

              require(dep.name == newBppb.name, s"${dep.name} => ${newBppb.name}")
              buildPerlPackageBlocks = buildPerlPackageBlocks - bppb.nixpkgsName + (newBppb.nixpkgsName -> newBppb)
              `perl-packages.nix` = `perl-packages.nix`.replace(bppb.source.trim, newBppb.source.trim)
            }

          case Right(bppb) => // insert new dep
            message ::= s"perlPackages.${bppb.nixpkgsName}: init at ${bppb.version}"

            buildPerlPackageBlocks += bppb.nixpkgsName -> bppb
            val after = (buildPerlPackageBlocks.until(bppb.nixpkgsName).lastOption getOrElse buildPerlPackageBlocks.last)._2
            if (CpanErrata.inExternalNixFiles contains bppb.name) {
              //`perl-packages.nix` = `perl-packages.nix`.replace(after.source.trim, after.source.trim+"\n/*\n  "+bppb.source.trim+"\n*/")
            } else {
              `perl-packages.nix` = `perl-packages.nix`.replace(after.source.trim, after.source.trim+"\n\n  "+bppb.source.trim)
            }
        }

        val pw = new java.io.PrintWriter(new File(repopath, "/pkgs/top-level/perl-packages.nix"))
        pw write `perl-packages.nix`
        pw.close()

        Some(message.reverse match {
               case first::Nil  => first
               case first::rest => first::""::"dependencies:"::rest.sorted mkString "\n"
             })
    }
  }
}





object Cpan2Nix {
  def main(args: Array[String]) {
    args match {
      case Array()                   => main(Array("--repopath", "./nixpkgs-repo"))
      case Array("--repopath", path) =>
        val repopath: File = new File(path)
        if (!repopath.exists) {
          require(Process("git" :: "clone" :: "https://github.com/nixos/nixpkgs" :: repopath.getAbsolutePath :: Nil).! == 0)
        } else {
          require(Process("git" :: "fetch" :: "origin" :: Nil, cwd = repopath).! == 0)
        }

        val branchName = { val now = new java.util.Date; f"cpan2nix-${1900+now.getYear}%04d-${1+now.getMonth}%02d-${now.getDate}%02d" }
        require(Process("git" :: "checkout"    ::         "remotes/origin/master"                     :: Nil, cwd = repopath).! == 0)
        require(Process("git" :: "branch"      :: "-f" :: branchName :: "HEAD"                        :: Nil, cwd = repopath).! == 0)
        require(Process("git" :: "checkout"    ::         branchName                                  :: Nil, cwd = repopath).! == 0)

        val nixPkgs = new NixPkgs(repopath.getAbsolutePath)

//      val nixPkgs = new NixPkgs("https://github.com/NixOS/nixpkgs/archive/staging.tar.gz")

        val canUpgradeMemo = collection.mutable.Map.empty[NixPackage, Option[CpanPackage]]
        def canUpgrade(np: NixPackage): Option[CpanPackage] = canUpgradeMemo.getOrElseUpdate(np, {
          CpanErrata.namesToIgnore.get(np.name) match {
            case Some(pred) if pred(np.version) =>
              None
            case _ =>
              CpanErrata.pinnedPackages find (_.name == np.name) orElse {
                np.maybeauthor match {
                  case Some(author) =>
                    Cpan.byAuthorAndName(author->np.name) match {
                      case cpps if cpps.isEmpty                         =>
                        Cpan.byName(np.name) match {
                          case cpps if cpps.isEmpty                          => val mod = Mod(np.name.toString.replace("-", "::")) // try to understand as module name
                                                                                Cpan.byMod(mod).toList match {
                                                                                  case cp::Nil => System.err.println(f"${np.url}%-90s not found in CPAN; but there is $mod in $cp");
                                                                                                  None // Some(cp)
                                                                                  case _       => System.err.println(f"${np.url}%-90s not found in CPAN");
                                                                                                  None
                                                                                }
                          case cpps if cpps exists (np.version <= _.version) => Some(cpps.maxBy(_.version))
                          case cpps if cpps forall (_.version < np.version)  => System.err.println(f"${np.url}%-90s not found in CPAN; there are only ${cpps.toArray mkString ", "}")
                                                                                Some(cpps.maxBy(_.version))
                        }
                      case cpps if cpps exists (np.version < _.version) =>
                        Some(cpps.maxBy(_.version))
                      case cpps if cpps exists (np.version == _.version)=>
                        Cpan.byName(np.name) match {
                          case cpps if cpps exists (np.version < _.version) => System.err.println(f"${np.url}%-90s other authors have newer versions ${cpps.filter(np.version < _.version).groupBy(_.author.toString).mapValues(_.map(_.version).toList.sorted) mkString ", "}")
                                                                               Some(cpps.maxBy(_.version))
                          case _                                            => Some(cpps.find(_.version == np.version).get)
                        }
                      case cpps if cpps forall (_.version < np.version) =>
                        Cpan.byName(np.name) match {
                          case cpps if cpps exists (np.version < _.version) => Some(cpps.maxBy(_.version))
                          case _                                            => System.err.println(f"${np.url}%-90s version not found in CPAN; there are only ${cpps.map(_.version).toArray.sorted mkString ", "}")
                                                                               Some(cpps.maxBy(_.version))
                        }
                    }
                  case None => // author is not specified in nixpkgs
                    Cpan.byName(np.name) match {
                     case cpps if cpps.isEmpty                          => throw new RuntimeException(s"${np.url} not found in CPAN")
                     case cpps if cpps exists (np.version <= _.version) => Some(cpps.maxBy(_.version))
                     case cpps if cpps forall (_.version < np.version)  => throw new RuntimeException(s"${np.url} not found in CPAN; there are only ${cpps.map(_.version).toArray.sorted mkString ", "}")
                   }
                }
              }
          }
        })


        val pullRequester = new PullRequester(repopath)

        val forceLocalBuild = true


        val toupdate = nixPkgs.allPackages sortBy { case np if np.name.toString equalsIgnoreCase "XML-SAX" => (0, 0)                  // XML-SAX first, it is an indirect dependency of many others via `pkgs.docbook'
                                                    case np if np.name.toString equalsIgnoreCase "JSON"    => (1, 0)                  // JSON second, others depends on it via `pkgs.heimdal'
                                                    case np                                                => canUpgrade(np) match {
                                                                                                                case Some(cp) => (10, pullRequester.allDeps(cp).size)  // then smaller first
                                                                                                                case None     => (20, 0)
                                                                                                              }
                                                  }
/*
        val toupdate = nixPkgs.allPackages filter (_.name == Name("autovivification"))
*/

        val totest = List.newBuilder[String]

        for (np      <- toupdate;
             cp      <- canUpgrade(np);
             message <- pullRequester.prepareCommit(np, cp)) {
          println("----")
          println(message)

          totest += pullRequester.nixifiedName(cp)

          Process("git" :: "commit" :: "-m" :: s"[cpan2nix] $message" :: "pkgs/top-level/perl-packages.nix" :: Nil,
                  cwd = repopath).!
        }


        // try to build
        val nixcode = s"""|let
                          |  pkgs = import <nixpkgs> { };
                          |in
                          |  [
                          |    ${totest.result flatMap (npname => List( s"pkgs.perlPackages.${npname}"
                                                                  // , s"pkgs.pkgsi686Linux.perlPackages.${npname}"
                                                                      )) mkString "\n    "}
                          |  ]
                          |""".stripMargin
        println(nixcode)
        val exitCode = Process("nix-build"
                            :: (if (forceLocalBuild)
                                     "--builders" :: ""
                                  :: "--option" :: "binary-caches" :: "http://nix-cache.s3.amazonaws.com/"
                                  :: Nil
                                else
                                     "--builders" :: "root@ifn2.dmz x86_64-linux /home/user/.ssh/id_ed25519 6 6 kvm,big-parallel"
                                  :: "--keep-going"
                                  :: Nil
                               )
                           ::: "--show-trace"
                            :: "--keep-failed" :: "-E" :: nixcode :: Nil,
                               cwd = repopath,
                               "NIXPKGS_CONFIG" -> "",
                               "NIX_PATH"       -> s"nixpkgs=${repopath.getAbsolutePath}"
                               ).!
        require(exitCode == 0)

      case _ =>
        println(s"unexpected args: ${args.toList}")
    }
  }
}
