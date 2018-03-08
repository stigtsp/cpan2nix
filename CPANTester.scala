#!/usr/bin/env perl
use File::Basename qw(dirname basename);
require("$ENV{HOME}/m/launcher/Launcher.pl.scala");

run(CLASS => 'CPANTester',
    TEE   => "cpantester.txt");
=cut
!#

import java.io.File
import java.nio.file.{Paths, Files}
import scala.sys.process._
import scala.util.{Try, Failure, Success}
//import scala.collection.immutable.TreeSet
import scala.collection.JavaConverters.asScalaSet
import ms.webmaster.launcher.position._

import `io.circe:circe-{generic,core,parser}_%SCALAVERSION%:0.9.1`
import `org.yaml:snakeyaml:1.20`
//import `commons-io:2.4`

//import `io.circe:circe-yaml_%SCALAVERSION%:0.7.0`
import io.circe._
import io.circe.generic.auto._



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
      case i   => val x = (a.vectorized(i), b.vectorized(i)) match {
                    case (c @ INT(ic), d @ INT(id)) => BigInt(ic) compare BigInt(id) match {
                                                         case 0 => ic compare id match {
                                                                     case 0 => c compare d
                                                                     case x => x // "00" and "000"
                                                                   }
                                                         case x => x
                                                       }
                    case (c          , d          ) => c compare d
                  }
                  require(x!=0, (a.vectorized(i), b.vectorized(i)))
                  x
    }
  }
}


class Author(s: String) {
  private val upcased: String = s.toUpperCase
  override def hashCode                = upcased.hashCode
  override def equals  (that: Any)     = that match { case that1: Author => upcased == that1.upcased }
  override def toString                = s
}
object Author {
  implicit def apply(s: String) = new Author(s)
}


class Name(s: String) /*extends Ordered[Version]*/ {
  private val upcased: String = s.toUpperCase
  override def hashCode                = upcased.hashCode
  override def equals  (that: Any)     = that match { case that1: Name => upcased == that1.upcased }
//override def compare (that: Name)    = upcased compare that.upcased
  override def toString                = s
}
object Name extends Ordering[Name] {
  implicit def apply(s: String) = new Name(s)
  def compare(a: Name, b: Name) = a.upcased compare b.upcased
}


class Pod(s: String) {
  private val upcased: String = s.toUpperCase
  override def hashCode                = upcased.hashCode
  override def equals  (that: Any)     = that match { case that1: Pod => upcased == that1.upcased }
  override def toString                = s
}
object Pod {
  implicit def apply(s: String) = new Pod(s)
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
       | "open_source"  | "http://opensource.org/licenses/Perl"                         => Set(new License("artistic1"), new License("gpl1Plus"))
    case "gpl_1"        | "http://opensource.org/licenses/gpl-license.php"
                        | "http://www.gnu.org/licenses/gpl.html"                        => Set(new License("gpl1"))
    case "gpl_2"        | "http://opensource.org/licenses/gpl-2.0.php"
                        | "http://www.gnu.org/licenses/old-licenses/gpl-2.0.txt"        => Set(new License("gpl2"))
    case "lgpl_2_1"     | "http://www.gnu.org/licenses/lgpl-2.1.html"
                        | "http://www.gnu.org/licenses/old-licenses/lgpl-2.1.txt"       => Set(new License("lgpl21"))
    case "lgpl_3_0"                                                                     => Set(new License("lgpl3"))
    case "gpl_3"        | "http://www.gnu.org/licenses/gpl-3.0.txt"                     => Set(new License("gpl3"))
    case                  "http://opensource.org/licenses/mozilla1.1.php"               => Set(new License("mpl11"))
    case "apache_2_0"                                                                   => Set(new License("asl20"))
    case "artistic_1"   | "http://opensource.org/licenses/artistic-license.php"         => Set(new License("artistic1"))
    case "artistic_2"   | "http://www.opensource.org/licenses/artistic-license-2.0.php"
                        | "http://www.opensource.org/licenses/artistic-license-2.0"
                        | "http://opensource.org/licenses/Artistic-2.0"
                        | "http://www.perlfoundation.org/artistic_license_2_0"          => Set(new License("artistic2"))
    case "mit"          | "http://www.opensource.org/licenses/mit-license.php"
                        | "http://opensource.org/licenses/mit-license.php"              => Set(new License("mit"))
    case                  "https://wiki.creativecommons.org/wiki/Public_domain"         => Set(new License("cc0"))
    case "unrestricted"                                                                 => Set(new License("wtfpl"))
    case "bsd"          | "http://opensource.org/licenses/bsd-license.php"              => Set(new License("bsd"))
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
    case re2(a, nameVersion) => val (n, v) = CpanErrata fromNameVersion nameVersion; NixPackage(Some(a), n, v, url)
    case re3(   nameVersion) => val (n, v) = CpanErrata fromNameVersion nameVersion; NixPackage(None   , n, v, url)
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
  lazy val tarballFile: File         = Cpan.downloadFile(this.path).get
  lazy val metaFile:    Option[File] = Cpan.downloadFile((NixPackage.extentions+"$").r.replaceAllIn(this.path, ".meta"  ))
//lazy val readmeFile:  Option[File] = Cpan.downloadFile((NixPackage.extentions+"$").r.replaceAllIn(this.path, ".readme"))
  lazy val sha256:      SHA256       = SHA256 fromString s"nix hash-file --type sha256 --base32 ${tarballFile}".!!.trim

  lazy val filesInTarball: Array[String] = if (tarballFile.getName endsWith ".zip")
                                             Array.empty // FIXME
                                           else
                                             s"tar --list --file $tarballFile".!! split '\n'
  lazy val isModule:       Boolean       = !name.toString.equalsIgnoreCase("Module-Build") && filesInTarball.contains(s"$name-$version/Build.PL")

  case class MetaExcerpt(runtimePODs: Set[Pod],
                         buildPODs:   Set[Pod],
                         description: Option[String],
                         licenses:    Set[License],
                         homepage:    Option[String])

  lazy val meta = {
    val metaContent: String = metaFile.fold("")(file => scala.io.Source.fromFile(file).mkString)
    var runtime =                                         Set.empty[String]
    var build   = if (isModule) Set("Module::Build") else Set.empty[String]
    if (metaContent startsWith "{") {
      val Right(json) = io.circe.parser.parse(metaContent)
      runtime ++= json.hcursor.downField("prereqs").downField("runtime"  ).downField("requires").keys.toList.flatten
      build   ++= json.hcursor.downField("prereqs").downField("configure").downField("requires").keys.toList.flatten
      build   ++= json.hcursor.downField("prereqs").downField("build"    ).downField("requires").keys.toList.flatten
    //build   ++= json.hcursor.downField("prereqs").downField("develop"  ).downField("requires").keys.toList.flatten
      build   ++= json.hcursor.downField("prereqs").downField("test"     ).downField("requires").keys.toList.flatten
      build   ++= json.hcursor.downField("prereqs").downField("test"     ).downField("suggests").keys.toList.flatten
      MetaExcerpt( runtime map (new Pod(_))
                 , build   map (new Pod(_))
                 , json.hcursor.downField("abstract").as[String].toOption
                 , json.hcursor.downField("resources").downField("license").as[Set[String]].getOrElse(Set.empty).flatMap(License.fromString(_))
                ++ json.hcursor.downField(                       "license").as[Set[String]].getOrElse(Set.empty).flatMap(License.fromString(_))
                 , json.hcursor.downField("resources").downField("homepage").as[String].toOption
                 )
    } else {
      val fixedmeta = metaContent.replace("author:       author:", "author:") // invalid yaml in "String-CamelCase-0.03.meta"
      val yaml: java.util.Map[String, Any] = new org.yaml.snakeyaml.Yaml load fixedmeta
      runtime ++= Try(asScalaSet(yaml.get("requires"          ).asInstanceOf[java.util.Map[String, String]].keySet).toSet).getOrElse(Set.empty)
      build   ++= Try(asScalaSet(yaml.get("configure_requires").asInstanceOf[java.util.Map[String, String]].keySet).toSet).getOrElse(Set.empty)
      build   ++= Try(asScalaSet(yaml.get("build_requires"    ).asInstanceOf[java.util.Map[String, String]].keySet).toSet).getOrElse(Set.empty)
      build   ++= Try(asScalaSet(yaml.get("x_test_requires"   ).asInstanceOf[java.util.Map[String, String]].keySet).toSet).getOrElse(Set.empty)
      MetaExcerpt( runtime map (new Pod(_))
                 , build   map (new Pod(_))
                 , Try(yaml.get("abstract").asInstanceOf[String]).toOption
                 , Try(License.fromString(yaml.get("resources").asInstanceOf[java.util.Map[String, String]].get("license"))).getOrElse(Set.empty)
                 , Try(                   yaml.get("resources").asInstanceOf[java.util.Map[String, String]].get("homepage")).toOption
                 )
    }
  }

  // todo: check min version
  private def filterDeps(deps: Set[CpanPackage]) = for (d <- deps if d != this;
                                                                  if !(d.name.toString.equalsIgnoreCase("Module-Build") && isModule);
                                                                  if !(CpanErrata.dependenciesToBreak(name) contains d.name))
                                                   yield d

  lazy val buildDeps:       Set[CpanPackage] = filterDeps(meta.buildPODs   ++ CpanErrata.extraBuildDependencies  (name) flatMap Cpan.podToPackage)
  lazy val runtimeDeps:     Set[CpanPackage] = filterDeps(meta.runtimePODs ++ CpanErrata.extraRuntimeDependencies(name) flatMap Cpan.podToPackage)

  private var _allDeps: Set[CpanPackage] = null
  def allDeps(parents: List[CpanPackage]=Nil): Set[CpanPackage] =
    if (_allDeps!= null)
      _allDeps
    else {
      if (parents contains this) {
        println(s"circular dependency ${(this::parents.takeWhile(_ != this):::this::Nil).reverse map (_.name) mkString " -> "}")
        Set.empty
      } else {
        _allDeps = runtimeDeps++buildDeps flatMap (d => d.allDeps(this :: parents) + d)
        _allDeps
      }
    }

  lazy val deepRuntimeDeps: Set[CpanPackage] = runtimeDeps flatMap (d => d.deepRuntimeDeps + d)

  def suggestedNixpkgsName: String = name.toString.replace("-", "")
}


object CpanPackage {
  private[this] val re = ("""[A-Z]/[A-Z]{2}/([A-Z0-9-]+)(?:/[^/]+)*/([^/]+)""" + NixPackage.extentions).r
  def fromPath(path: String) = path match {
    case re(author, nameVersion) => val (name, version) = CpanErrata fromNameVersion nameVersion
                                    CpanPackage(author, name, version, path)
  }
}


object CpanErrata {
  // *** difficult parse cases
  private[this] val exception1 = "(PerlMagick)-([0-9.]+-[0-9.]+)".r
  private[this] val rule1      = "([^/]+)-([^/-]+)".r
  def fromNameVersion(s: String): (Name, Version) = s match {
    case "Data-Dumper-Lazy"                 => (s,                                 new Version("0"))
    case "Spreadsheet-ParseExcel-Assist"    => (s,                                 new Version("0"))
    case "Spreadsheet-WriteExcel-WebPivot2" => ("Spreadsheet-WriteExcel-WebPivot", new Version("2"))
    case exception1(n, v)                   => (n,                                 new Version(v))
    case rule1     (n, v)                   => (n,                                 new Version(v))
  }

  val podsToIgnore             = Set[Pod ]           ( "perl", "Config", "Errno", "File::Temp" // <- provided By Perl
                                                     )
  val namesToIgnore            = Set[Name]           ( "perl"
                                                     , "PathTools"                             // breaks the installPhase
                                                     , "if", "Digest-SHA", "autodie"           // <- banned (.. = null) in nixpkgs
                                                     , "Mac-SystemDirectory"
                                                     )

  // *** hack to work with packages wich are out of perl-packages.nix
  val inExternalNixFiles       = Set[Name]           ( "Compress-Raw-Zlib"                     // in an external file (todo? move into perl-packages.nix)
                                                     )

  // *** do not add to nixpkgs dependencies present on cpan
  val dependenciesToBreak      = Map[Name, Set[Name]]( new Name("ExtUtils-MakeMaker"      ) -> Set[Name]("podlators", "Encode", "PathTools", "Data-Dumper")         // circular dependency
                                                     , new Name("Test-Simple"             ) -> Set[Name]("PathTools", "File-Temp", "Scalar-List-Utils", "Storable") // circular dependency
                                                     , new Name("Exporter"                ) -> Set[Name]("Carp")                                                    // circular dependency
                                                     , new Name("Test-CleanNamespaces"    ) -> Set[Name]("Moose", "MooseX-Role-Parameterized")                      // circular dependency
                                                     , new Name("File-ShareDir"           ) -> Set[Name]("File-ShareDir-Install")                                   // circular dependency
                                                     //MooX-ConfigFromFile -> MooX-Options
                                                     ) withDefaultValue Set.empty

  // *** add to nixpkgs dependencies missing on cpan (usually due to missing .meta file)
  val extraBuildDependencies   = Map[Name, Set[Pod ]]( new Name("CPAN"                    ) -> Set[Pod]("Archive::Zip")
                                                     , new Name("Autodia"                 ) -> Set[Pod]("DBI")
                                                     , new Name("Data-Page-Pageset"       ) -> Set[Pod]("Data::Page")
                                                     , new Name("Data-Taxi"               ) -> Set[Pod]("Debug::ShowStuff")
                                                     , new Name("DateTime-Calendar-Julian") -> Set[Pod]("DateTime")
                                                     , new Name("Font-TTF"                ) -> Set[Pod]("IO::String")
                                                     , new Name("Gnome2-Canvas"           ) -> Set[Pod]("ExtUtils::Depends")
                                                     , new Name("HTML-Selector-XPath"     ) -> Set[Pod]("Test::Base")
                                                     , new Name("Module-Info"             ) -> Set[Pod]("Test::Pod", "Test::Pod::Coverage")
                                                     , new Name("PerlIO-via-symlink"      ) -> Set[Pod]("inc::Module::Install")
                                                     ) withDefaultValue Set.empty
  val extraRuntimeDependencies = Map[Name, Set[Pod ]]( new Name("Any-Moose"               ) -> Set[Pod]("Mouse", "Moose")
                                                     , new Name("GDTextUtil"              ) -> Set[Pod]("GD")
                                                     , new Name("Linux-Inotify2"          ) -> Set[Pod]("common::sense")
                                                     , new Name("Log-LogLite"             ) -> Set[Pod]("IO::LockedFile")
                                                     ) withDefaultValue Set.empty

  // *** pinned packages
  val dontUpgrade              = List[CpanPackage]   ( CpanPackage fromPath "D/DA/DAGOLDEN/CPAN-Meta-2.150005.tar.gz"
                                                     , CpanPackage fromPath "N/NJ/NJH/MusicBrainz-DiscID-0.03.tar.gz"  // need to fix patchPhase manually
//                                                   , CpanPackage fromPath "G/GR/GRANTM/XML-SAX-0.96.tar.gz"          // 1.00 does not pass tests?
                                                     )

  // *** enforce 'doCheck = false'
  val dontCheck                = Set[Name]           ( "PathTools"
                                                     , "Net-HTTP"
                                                     , "B-C" // ok but slow
                                                     )
}


object Cpan {
  private val allPackages     = new collection.mutable.HashMap[CpanPackage, CpanPackage]
          val byPod           = new collection.mutable.HashMap[Pod,            Set[CpanPackage]] withDefaultValue Set.empty
          val byName          = new collection.mutable.HashMap[Name,           Set[CpanPackage]] withDefaultValue Set.empty
          val byAuthorAndName = new collection.mutable.HashMap[(Author, Name), Set[CpanPackage]] withDefaultValue Set.empty

  locally {
    if (!new File("02packages.details.txt").exists) {
      "wget https://raw.githubusercontent.com/metacpan/metacpan-cpan-extracted/master/02packages.details.txt".!
    }
    val re = ("""(\S+)\s+\S+\s+([A-Z]/[A-Z]{2}/[A-Z0-9-]+(?:/[^/]+)*/[^/]+-[^/-]+""" + NixPackage.extentions + ")").r
    scala.io.Source.fromFile("02packages.details.txt").getLines dropWhile(_.nonEmpty) dropWhile(_.isEmpty) foreach {
      case re(pod, path) => Try(CpanPackage fromPath path) match {
                              case Success(cp) =>
                                val cpinterned = allPackages.getOrElseUpdate(cp, cp)
                                byPod          (               pod) += cpinterned
                                byName         (           cp.name) += cpinterned
                                byAuthorAndName(cp.author->cp.name) += cpinterned
                              case Failure(e) =>
                                System.err.println(s"ignore `$path' $e")
                            }
      case line          => //System.err.println(s"ignore `$line'")
    }
  }

  def podToPackage(pod: Pod): Option[CpanPackage] =
    if (CpanErrata.podsToIgnore contains pod)
      None
    else
      byPod(pod).toList match {
        case Nil                                                  => println(s"pod `$pod' not found!!!"); ???
        case cp::Nil if CpanErrata.namesToIgnore contains cp.name => None
        case cp::Nil                                              => CpanErrata.dontUpgrade.find(_.name == cp.name) match {
                                                                       case Some(pinnedcp) => Some(pinnedcp)
                                                                       case None           => Some(cp)
                                                                     }
        case cpps                                                 => println(s"pod `$pod' provided by many $cpps"); ???
      }

  def downloadFile(path: String): Option[File] = {
    val localfilename     = new File(s"${__DIR__}/spool/${Paths.get(path).getFileName}")
    val localfilename_tmp = new File(s"${__DIR__}/spool/${Paths.get(path).getFileName}.tmp")
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

    val (name, version) = CpanErrata.fromNameVersion(resolvedNameAndVersion)

    def copy( builder:               String
            , versionString:         String
            , nameAndVersion:        String
            , url:                   String
            , sha256:                SHA256
            , doCheck:               Option[String]
            , buildInputs:           Traversable[String]
            , propagatedBuildInputs: Traversable[String]
            , licenses:              Traversable[License]
            ): BuildPerlPackageBlock = {
      var s = source
      s = """\bbuildPerl(Package|Module)\b"""   .r.replaceAllIn(s, builder)
      s = """(?s)version\s*=\s*"[^"]+";"""      .r.replaceAllIn(s, s"version = \042${versionString}\042;")
      s = """(?s)name\s*=\s*"[^"]+";"""         .r.replaceAllIn(s, s"name = \042${nameAndVersion}\042;")

      s = """(?s)url\s*=\s*"?([^";]+)"?;"""     .r.replaceAllIn(s, s"url = ${url};")

      s = """(?s)sha256\s*=\s*"([a-z0-9]+)";""" .r.replaceSomeIn(s, m => m.group(1).length match {
                                                                       //case _ if sha256 == this.sha256 => None
                                                                       case 64 => Some(s"sha256 = \042${sha256.base16}\042;")
                                                                       case 52 => Some(s"sha256 = \042${sha256.base32}\042;")
                                                                     })
      (this.doCheck, doCheck) match {
        case (None,      Some(text)) => val a = s.split('\n')
                                        s = (a.init :+ s"    doCheck = ${text};" :+ a.last) mkString "\n"
        case (`doCheck`, _         ) =>
        case (Some(_),   None      ) => ???
        case (Some(_),   Some(text)) => s = """(?s) doCheck\s*=\s*[^;]+;""".r.replaceAllIn(s, s" doCheck = ${text};")
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
        case (None   , true ) => // FIXME: insert licenses which were absent
        case (Some(_), false) => // FIXME: remove licenses which were present
        case (Some(_), true ) => s = """(?s) license\s*=\s*(with[^;]+;\s*)[^;]+;""".r.replaceAllIn(s, s" license = with stdenv.lib.licenses; [ ${licenses mkString " "} ];")
      }

      // FIXME: update homepage
      new BuildPerlPackageBlock(s)
    }

    def updatedTo(cp: CpanPackage): BuildPerlPackageBlock =
      copy( builder               = if (cp.isModule) "buildPerlModule" else "buildPerlPackage"
          , versionString         = cp.version.toString
          , nameAndVersion        = s"${cp.name}-${cp.version}"
          , url                   = s"mirror://cpan/authors/id/${cp.path}"
          , sha256                = cp.sha256
          , doCheck               = if (CpanErrata.dontCheck contains cp.name) Some("false") else doCheck
          , propagatedBuildInputs = propagatedBuildInputs.toList.flatten.filter(_ startsWith "pkgs.") ++ (cp.runtimeDeps -- cp.runtimeDeps.flatMap(_.deepRuntimeDeps)).map(nixifiedName).toArray.sorted
          , buildInputs           =           buildInputs.toList.flatten.filter(_ startsWith "pkgs.") ++ (cp.buildDeps   -- cp.deepRuntimeDeps                       ).map(nixifiedName).toArray.sorted
          , licenses              = cp.meta.licenses
          )

    def this(cp: CpanPackage) = this {
      val sb = new java.lang.StringBuilder
      sb append s"""  ${cp.suggestedNixpkgsName} = ${if (cp.isModule) "buildPerlModule" else "buildPerlPackage"} rec {\n"""
      sb append s"""     version = "${cp.version}";\n"""
      sb append s"""     name = "${cp.name}-${cp.version}";\n"""
      sb append s"""     src = fetchurl {\n"""
      sb append s"""       url = mirror://cpan/authors/id/${cp.path};\n"""
      sb append s"""       sha256 = "${cp.sha256.base32}";\n"""
      sb append s"""     };\n"""
      (cp.runtimeDeps -- cp.runtimeDeps.flatMap(_.deepRuntimeDeps)).toArray match {
        case Array() =>
        case a       => sb append s"""     propagatedBuildInputs = [ ${a.map(nixifiedName).sorted mkString " "} ];\n"""
      }
      (cp.buildDeps   -- cp.deepRuntimeDeps                       ).toArray match {
        case Array() =>
        case a       => sb append s"""     buildInputs = [ ${a.map(nixifiedName).sorted mkString " "} ];\n"""
      }
      if (CpanErrata.dontCheck contains cp.name) {
        sb append s"""       doCheck = false;\n"""
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
  val buildPerlPackageBlocks = new collection.mutable.TreeMap[Name, BuildPerlPackageBlock]()(Name)
  for (source <- """(?s)  \S+\s*=\s*(let .+? in\s*)?buildPerl(Package|Module) (rec )?\{.+?\n  \};\n""".r.findAllIn(`perl-packages.nix`);
       bppb <- Try(new BuildPerlPackageBlock(source)) /*match { case Success(b) => Success(b); case Failure(e) => println(source); e.printStackTrace(); Failure(e) }*/ ) {
    buildPerlPackageBlocks += bppb.name -> bppb
  }

  def nixifiedName(cp: CpanPackage) = {
    val s =  buildPerlPackageBlocks.get(cp.name) match {
      case Some(bppb) => bppb.nixpkgsName
      case None       => cp.suggestedNixpkgsName
    }
    s match {
      case "version" => "self.version"
      case "if"      => "self.\"if\""
      case _         => s
    }
  }

  def prepareCommit(np: NixPackage, cp: CpanPackage): Option[String] = {
    cp.allDeps() // to fail earlier if circular deps found

    println(s"prepareCommit($np, $cp)")
    println(cp.allDeps() mkString "\n")
    println(s"buildPODs:")
    println(cp.meta.buildPODs mkString "\n")
    println(s"runtimePODs:")
    println(cp.meta.runtimePODs mkString "\n")
    /*
    */

    var message = List.empty[String]
    if (np.version == cp.version)
      message ::= s"${np.name}: cleanup"
    else
      message ::= s"${np.name}: ${np.version} -> ${cp.version}"



    buildPerlPackageBlocks find (_._2.resolvedUrl == np.url) match {
      case None =>
        System.err.println(s"$np->$cp not found in perl-packages.nix")
        None
      case Some((_, block)) =>
        val newBlock = block.updatedTo(cp)
        val depBlocks: Set[Either[(BuildPerlPackageBlock, CpanPackage), BuildPerlPackageBlock]] =
          for (dep <- cp.allDeps()) yield
            buildPerlPackageBlocks.get(dep.name) match {
              case Some(bppb) => (Left(bppb->dep))
              case None       => (Right(new BuildPerlPackageBlock(dep))) // Try(new BuildPerlPackageBlock(dep)).toOption.map(Right(_))
            }


        // debug print
        println(s"======================== ${np.name}: ${np.version} -> ${cp.version}")
        println(block.source)
        depBlocks foreach {
          case Left((bppb, dep)) => // upgrade/cleanup existing dep
            val newBppb = bppb.updatedTo(dep)
            if (bppb.source.trim != newBppb.source.trim)
              println(bppb.source)
          case _ =>
        }
        println(s"------------------------")
        println(newBlock.source)
        depBlocks foreach {
          case Left((bppb, dep)) => // upgrade/cleanup existing dep
            val newBppb = bppb.updatedTo(dep)
            if (bppb.source.trim != newBppb.source.trim)
              println(newBppb.source)
          case Right(bppb) =>
              println(bppb.source)
        }


        // do mutate `perl-packages.nix`
        buildPerlPackageBlocks(block.name) = newBlock
        `perl-packages.nix` = `perl-packages.nix`.replace(block.source.trim, newBlock.source.trim)

        depBlocks foreach {
          case Left((bppb, dep)) => // upgrade/cleanup existing dep
            val newBppb = bppb.updatedTo(dep)
            if (bppb.source.trim != newBppb.source.trim) {
              if (bppb.version != newBppb.version)
                message ::= s"${bppb.name}: ${bppb.version} -> ${newBppb.version}"
              else
                message ::= s"${bppb.name}: cleanup"

              require(dep.name == newBppb.name, s"${dep.name} => ${newBppb.name}")
              buildPerlPackageBlocks(dep.name) = newBppb
              `perl-packages.nix` = `perl-packages.nix`.replace(bppb.source.trim, newBppb.source.trim)
            }

          case Right(bppb) => // insert new dep
            message ::= s"${bppb.name}: init at ${bppb.version}"

            buildPerlPackageBlocks(bppb.name) = bppb
            val after = (buildPerlPackageBlocks.until(bppb.name).lastOption getOrElse buildPerlPackageBlocks.last)._2
            if (CpanErrata.inExternalNixFiles contains bppb.name) {
              `perl-packages.nix` = `perl-packages.nix`.replace(after.source.trim, after.source.trim+"\n/*\n  "+bppb.source.trim+"\n*/")
            } else {
              `perl-packages.nix` = `perl-packages.nix`.replace(after.source.trim, after.source.trim+"\n/*+*/\n  "+bppb.source.trim)
            }
        }

        val pw = new java.io.PrintWriter(new File(repopath, "/pkgs/top-level/perl-packages.nix"))
        pw write `perl-packages.nix`
        pw.close()

        Some(message.reverse match {
               case first::Nil  => first
               case first::rest => first::""::"dependencies:"::rest mkString "\n"
             })
    }
  }
}





object CPANTester {
  def main(args: Array[String]) {

    val mayberepopath: Option[File] = (args.sliding(2,1) flatMap {
                                           case Array("--fix-repo", repopath) => Some(new File(repopath))
                                           case _                             => None
                                        }).toList.headOption

    val nixPkgs = mayberepopath match {
      case None => // only report
        new NixPkgs("https://github.com/NixOS/nixpkgs/archive/staging.tar.gz")
      case Some(repopath) =>
//==    Process("git" :: "fetch"    ::         "nixpkgs"                          :: Nil, cwd = repopath).!
        Process("git" :: "checkout" :: "-f" :: "remotes/nixpkgs/staging"          :: Nil, cwd = repopath).!
        Process("git" :: "branch"   :: "-f" :: "perl-packages-update-3" :: "HEAD" :: Nil, cwd = repopath).!
        Process("git" :: "checkout" ::         "perl-packages-update-3"           :: Nil, cwd = repopath).!
        new NixPkgs(repopath.getAbsolutePath)
    }


    case class Result( upgradable: List[(NixPackage, CpanPackage)],
//                   , report:     String
                     )

    val result = {
      val up = List.newBuilder[(NixPackage, CpanPackage)]
      nixPkgs.allPackages foreach {
        case np @ NixPackage(maybeauthor, name, version, url) =>
          //if (!url.startsWith("mirror:")) {
          //  out append s"| $url | the url does not use mirror:// scheme\n"
          //}
          maybeauthor match {
            case Some(author) =>
              Cpan.byAuthorAndName(author->name) match {
                case cpps if cpps.isEmpty =>
                  Cpan.byName(name) match {
                    case cpps if cpps.isEmpty =>
                      // try to understand as POD
                      Cpan.byPod(new Pod(name.toString.replace("-", "::"))).toList match {
                        case cp::Nil => up += np -> cp
                        case _       => println(s"| $url | the package is not found in CPAN")
                      }

                    case cpps if cpps.exists(version <= _.version) =>
                      up += np -> cpps.maxBy(_.version)

                    case cpps if cpps.forall(_.version < version) =>
                      println(s"$url not found in CPAN; there are only ${cpps.toArray mkString ", "}\n")
                      up += np -> cpps.maxBy(_.version)
                   }

                case cpps if cpps.exists(version < _.version) =>
                  up += np -> cpps.maxBy(_.version)

                case cpps if cpps.exists(version == _.version) =>
                  Cpan.byName(name) match {
                    case cpps if cpps.exists(version < _.version) =>
                      //out append  s"|      | the newer versions ${cpps.filter(version < _.version).map(_.version).toArray.sorted mkString ", "} found in CPAN under authors ${cpps.filter(version < _.version).map(_.author.toString).toArray.sorted mkString ", "}\n"
                      println(s"$url: other authors may have newer versions ${cpps.filter(version < _.version).groupBy(_.author.toString).mapValues(_.map(_.version).toList.sorted) mkString ", "}")
                    case _ =>
                  }
                  up += np -> cpps.find(_.version == version).get

                case cpps if cpps.forall(_.version < version) =>
                  Cpan.byName(name) match {
                    case cpps if cpps.exists(version < _.version) =>
                      up += np -> cpps.maxBy(_.version)
                    case _ =>
                      println(s"$url: version $version not found in CPAN; there are only ${cpps.map(_.version).toArray.sorted mkString ", "}")
                      up += np -> cpps.maxBy(_.version)
                  }
              }
            case None => // author is not specified in nixpkgs
              Cpan.byName(name) match {
                case cpps if cpps.isEmpty =>
                  println(s"$url not found in CPAN")
                  ???

                case cpps if cpps.exists(version <= _.version) =>
                  up += np -> cpps.maxBy(_.version)

                case cpps if cpps.forall(_.version < version) =>
                  println(s"$url not found in CPAN; there are only ${cpps.map(_.version).toArray.sorted mkString ", "}")
                  ???
              }
          }
      }
      Result(up.result)
    }

    val markdown = new java.io.PrintWriter("report.md")
//  markdown.write(result.report)
    markdown.write(result.upgradable mkString "\n")
    markdown.close()


    mayberepopath match {
      case None =>
      case Some(repopath) =>
        val pullRequester = new PullRequester(repopath)

        for ((np, cp1) <- result.upgradable sortBy { case (np, cp) if cp.name.toString equalsIgnoreCase "XML-SAX" => (0, 0)                  // XML-SAX first, it is an indirect dependency of many others via docbook
                                                     case (np, cp)                                                => (1, cp.allDeps().size)  // then smaller first
                                                   }
//           if cp1.name.toString.equalsIgnoreCase("PerlMagick")
             ) {
          val cp2 = CpanErrata.dontUpgrade.find(_.name == cp1.name) getOrElse cp1
          pullRequester.prepareCommit(np, cp2) match {
            case Some(message) =>
              println(message)
              // try to build
              val nixcode = s"""|let
                                |  pkgs = import <nixpkgs> { config.allowBroken = true; };
                                |in
                                |  pkgs.perlPackages.${pullRequester.nixifiedName(cp2)}
                                |""".stripMargin
              val exitCode = Process("nix-build" :: /*"--builders" :: "" ::*/ "--show-trace" :: "-K" :: "-E" :: nixcode :: Nil,
                                     cwd = repopath,
                                     "NIXPKGS_CONFIG" -> "",
                                     "NIX_PATH"       -> s"nixpkgs=${repopath.getAbsolutePath}"
                                     ).!
              require(exitCode == 0)
//            Process("git" :: "commit" :: "-m" :: message :: "pkgs/top-level/perl-packages.nix" :: Nil,
//                    cwd = new java.io.File(repopath)).!
            case None =>
          }
        }
    }

  }
}
