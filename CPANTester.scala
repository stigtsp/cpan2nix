#!/usr/bin/env perl
use File::Basename qw(dirname basename);
require("$ENV{HOME}/m/launcher/Launcher.pl.scala");

run(CLASS => 'CPANTester',
    TEE   => "cpantester.txt");
=cut
!#

import java.nio.file.{Path, Paths, Files}
import scala.sys.process._
import scala.util.{Try, Failure, Success}

class Version(s: String) extends Ordered[Version] {
  private lazy val vectorized: Array[String] = s stripPrefix "v" split "\\s+|(?=[0-9])(?<=[^0-9])|(?=[^0-9])(?<=[0-9])"
  override def hashCode                = vectorized.foldLeft(0)(_ * 31 + _.hashCode)
  override def equals  (that: Any)     = that match { case that1: Version => compare(that1) == 0 case _ => false }
  override def compare (that: Version) = Version.compare(this, that)
  override def toString                = s
}
object Version extends Ordering[Version] {
  implicit def apply(s: String) = new Version(s)

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


case class NixPerlPackage(maybeauthor: Option[String], name: String, version: Version, url: String)

object NixPerlPackage {
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
  private[this] val re2 = (mirrors + """/+authors/id/[A-Z0-9-]/[A-Z0-9-]{2}/([A-Z0-9-]+)(?:/[^/]+)*/+([^/]+)-([^/-]+)""" + extentions).r
  private[this] val re3 = (mirrors + """/+modules/by-module/[^/]+/([^/]+)-([^/-]+)"""                                    + extentions).r
  def fromString(url: String) = url match {
    case re2(a, n, v) => NixPerlPackage(Some(a), n, v, url)
    case re3(   n, v) => NixPerlPackage(None   , n, v, url)
  }
}


class NixPkgs(repopath: String) {
  lazy val allPerlPackages: List[NixPerlPackage] = {

    val output = Process("nix-instantiate" :: "--show-trace" :: "--strict" :: "--eval" :: "-E" ::
                         """|with (import <nixpkgs> { config.allowBroken = true; });
                            |lib.concatMap (x: x.src.urls or []) (lib.attrValues perlPackages)
                            |""".stripMargin :: Nil,
                         None,
                         "NIXPKGS_CONFIG" -> "",
                         "NIX_PATH"       -> s"nixpkgs=${repopath}"
                         ).!!

    "\"([^ \"]+)\"".r.findAllMatchIn(output).map(_ group 1).toList.distinct flatMap { url =>
      Try(NixPerlPackage.fromString(url)) match {
        case Failure(_)  => //System.err.println(s"ignore non-CPAN url $url")
                            None
        case Success(ca) => Some(ca)
      }
    }
  }
}



case class CpanPerlPackage(author: String, name: String, version: Version, path: String)

object CpanOrg {
  lazy val allPerlPackages: List[CpanPerlPackage] = {
    if (!Files.exists(Paths get "02packages.details.txt")) {
      "wget https://raw.githubusercontent.com/metacpan/metacpan-cpan-extracted/master/02packages.details.txt".!
    }
    val re = (""".+ ([A-Z]/[A-Z]{2}/([A-Z0-9-]+)(?:/[^/]+)*/([^/]+)-([^/-]+)""" + NixPerlPackage.extentions + ")").r
    scala.io.Source.fromFile("02packages.details.txt").getLines.dropWhile(_.nonEmpty).dropWhile(_.isEmpty).flatMap{
      case re(path, a, n, v) => Some(CpanPerlPackage(a, n, v, path))
      case line              => //System.err.println(s"ignore $line")
                                None
    }.toList.distinct
  }
}


class Patcher(repopath: String) {
  class BuildPerlPackageBlock(val source: String) {
    lazy val version:      Option[String] = """(?s)version\s*=\s*"([^"]+)";"""    .r.findFirstMatchIn(source).map(_ group 1)
    lazy val name:         Option[String] = """(?s)name\s*=\s*"([^"]+)";"""       .r.findFirstMatchIn(source).map(_ group 1)
    lazy val url:          Option[String] = """(?s)url\s*=\s*"?([^";]+)"?;"""     .r.findFirstMatchIn(source).map(_ group 1)
    lazy val sha256:       Option[String] = """(?s)sha256\s*=\s*"([a-z0-9]+)";""" .r.findFirstMatchIn(source).map(_ group 1)
    lazy val resolvedName: Option[String] = name map (n => version.fold(n)(n.replace("${version}", _)))
    lazy val resolvedUrl:  Option[String] = url  map (n => version.fold(n)(n.replace("${version}", _))) map (n => resolvedName.fold(n)(n.replace("${name}", _)))

    def copy(version: String = null, name: String = null, url: String = null, sha256: String = null): BuildPerlPackageBlock = {
      var s = source
      if (version != null) s = """(?s)version\s*=\s*"([^"]+)";"""   .r.replaceAllIn(s, s"version = \042${version}\042;")
      if (name    != null) s = """(?s)name\s*=\s*"([^"]+)";"""      .r.replaceAllIn(s, s"name = \042${name}\042;")
      if (url     != null) s = """(?s)url\s*=\s*"?([^";]+)"?;"""    .r.replaceAllIn(s, s"url = \042${url}\042;")
      if (sha256  != null) s = """(?s)sha256\s*=\s*"([a-z0-9]+)";""".r.replaceAllIn(s, s"sha256 = \042${sha256}\042;")
      new BuildPerlPackageBlock(s)
    }
  }

  var `perl-packages.nix` = scala.io.Source.fromFile(s"${repopath}/pkgs/top-level/perl-packages.nix").mkString
  val buildPerlPackageBlocks: List[BuildPerlPackageBlock] = """(?s)=.+?buildPerl(Package|Module) (rec )?\{.+?\n  \};\n""".r.findAllIn(`perl-packages.nix`).map(new BuildPerlPackageBlock(_)).toList

  def doFix(npp: NixPerlPackage, cpp: CpanPerlPackage): Boolean = {
    buildPerlPackageBlocks find (_.resolvedUrl == Some(npp.url)) match {
      case None =>
        System.err.println(s"$npp not found in perl-packages.nix")
        false
      case Some(block) =>
        val localfilename = Paths.get("spool") resolve Paths.get(cpp.path).getFileName
        if (!Files.exists(localfilename)) {
          s"wget -O ${localfilename} https://cpan.metacpan.org/authors/id/${cpp.path}".!
        }
        val newBlock = block.copy(version = cpp.version.toString,
                                  name    = s"${cpp.name}-${cpp.version}",
                                  url     = s"mirror://cpan/authors/id/${cpp.path}",
                                  sha256  = s"nix hash-file --type sha256 --base32 ${localfilename}".!!.trim)
//      println(block.source)
//      println(newBlock.source)

        `perl-packages.nix` = `perl-packages.nix`.replace(block.source, newBlock.source)
        val pw = new java.io.PrintWriter(s"${repopath}/pkgs/top-level/perl-packages.nix")
        pw write `perl-packages.nix`
        pw.close()

        true
    }
  }
}


object CPANTester {

  def main(args: Array[String]) {
//  for (b <- new Patcher("/home/user/cd/nixpkgs").buildPerlPackageBlocks) {
//    println(b.source)
//  }

    val mayberepopath: Option[String] = (args.sliding(2,1) flatMap {
                                           case Array("--fix-repo", repopath) => Some(repopath)
                                           case _                             => None
                                        }).toList.headOption

    val nixPkgs = new NixPkgs(mayberepopath getOrElse "https://github.com/NixOS/nixpkgs/archive/staging.tar.gz")

    for (repopath <- mayberepopath) {
      Process("git" :: "checkout" :: "-f" :: "remotes/nixpkgs/staging"          :: Nil, cwd = new java.io.File(repopath)).!
      Process("git" :: "pull"     ::         "nixpkgs"                          :: Nil, cwd = new java.io.File(repopath)).!
      Process("git" :: "branch"   :: "-f" :: "perl-packages-update-3" :: "HEAD" :: Nil, cwd = new java.io.File(repopath)).!
      Process("git" :: "checkout" ::         "perl-packages-update-3"           :: Nil, cwd = new java.io.File(repopath)).!
    }


    val cpanByName:          Map[String,           List[CpanPerlPackage]] = CpanOrg.allPerlPackages groupBy (_.name.toUpperCase)                      withDefaultValue Nil
    val cpanByAuthorAndName: Map[(String, String), List[CpanPerlPackage]] = CpanOrg.allPerlPackages groupBy (cpp => cpp.author->cpp.name.toUpperCase) withDefaultValue Nil

    val markdown = new java.io.PrintWriter("report.md")
    markdown println s"| nixpkgs url | problem |"
    markdown println s"| ---- | ---- |"
    val patches: List[(NixPerlPackage, CpanPerlPackage)] = nixPkgs.allPerlPackages flatMap {
      case npp @ NixPerlPackage(maybeauthor, name, version, url) =>
        if (!url.startsWith("mirror:")) {
          markdown println s"| $url | the url does not use mirror:// scheme"
        }
        maybeauthor match {
          case Some(author) =>
            cpanByAuthorAndName(author->name.toUpperCase) match {
              case Nil       =>
                cpanByName(name.toUpperCase) match {
                  case Nil  =>
                    markdown println s"| $url | the package not found in CPAN"
                    None
                  case cpps if cpps.exists(_.author != cpps.head.author) =>
                    markdown println s"| $url | the package not found in CPAN under author ${author} but now it has many authors ${cpps.groupBy(_.author).mapValues(_ map (_.version)) mkString ", "}; please specify one in nixpkgs"
                    None
                  case cpps if cpps.exists(version < _.version) =>
                    markdown println s"| $url | the package not found in CPAN under author ${author} but ${cpps.head.author}; please update in nixpkgs if its correct"
                    markdown println s"|      | can be updated to ${cpps.map(_.version).filter(_ > version).sorted mkString ", "}"
                    //patcher foreach (_.doCommit(npp, cpps.maxBy(_.version)))
                    Some(npp -> cpps.maxBy(_.version))
                  case cpps if cpps.forall(version != _.version) =>
                    markdown println s"| $url | the package not found in CPAN under author ${author} but ${cpps.head.author}; please update in nixpkgs if its correct"
                    markdown println s"|      | version $version used in nixpkgs not found in CPAN; there are only ${cpps.map(_.version).sorted mkString ", "}"
                    Some(npp -> cpps.maxBy(_.version))
                  case cpps =>
                    markdown println s"| $url | the package not found in CPAN under author ${author} but ${cpps.head.author}; please update in nixpkgs if its correct"
                    Some(npp -> cpps.find(_.version == version).get)
                 }
              case cpps if cpps.exists(version < _.version) =>
                markdown println s"| $url | $version can be updated to ${cpps.map(_.version).filter(_ > version).sorted mkString ", "}"
                Some(npp -> cpps.maxBy(_.version))
              case cpps if cpps.forall(version != _.version) =>
                markdown println s"| $url | version $version used in nixpkgs not found in CPAN; there are only ${cpps.map(_.version).sorted mkString ", "}"
                cpanByName(name.toUpperCase) match {
                  case cpps if cpps.exists(version < _.version) =>
                    markdown println s"|      | the newer versions ${cpps.filter(version < _.version).map(_.version).distinct.sorted mkString ", "} found in CPAN under authors ${cpps.filter(version < _.version).map(_.author).distinct.sorted mkString ", "}"
                    Some(npp -> cpps.maxBy(_.version))
                  case _ =>
                    None
                }
              case cpps =>
                //markdown println s"| $url | version $version is up to date"
                None
            }
          case None =>
            cpanByName(name.toUpperCase) match {
              case Nil       =>
                markdown println s"| $url | package $name not found in CPAN"
                None
              case cpps if cpps.exists(_.author != cpps.head.author) =>
                markdown println s"| $url | package $name has many authors in CPAN ${cpps.map(_.author).distinct mkString ", "}; please specify one in nixpkgs"
                None
              case cpps if cpps.exists(version < _.version) =>
                markdown println s"| $url | author ${cpps.head.author} not specified in nixpkgs"
                markdown println s"|      | can be updated to ${cpps.map(_.version).filter(_ > version).sorted mkString ", "}"
                Some(npp -> cpps.maxBy(_.version))
              case cpps if cpps.forall(version != _.version) =>
                markdown println s"| $url | author ${cpps.head.author} not specified in nixpkgs"
                markdown println s"|      | version $version used in nixpkgs not found in CPAN; there are only ${cpps.map(_.version).sorted mkString ", "}"
                None
              case cpps =>
                //markdown println s"| $url | version $version is up to date"
                Some(npp -> cpps.find(version == _.version).get)
            }
        }
      }
    markdown.close()


    for (repopath <- mayberepopath) {
      val patcher = new Patcher(repopath)
      var bigCommitMessage = "perl packages update:\n\n"
      for ((npp, cpp) <- patches) {
        if (patcher.doFix(npp,cpp)) {
          bigCommitMessage += (if (npp.version == cpp.version)
                                 s"${npp.name}: fix link\n"
                               else
                                 s"${npp.name}: ${npp.version} -> ${cpp.version}\n")
        }
        if (bigCommitMessage.count(_ == '\n') >= 20) {
          Process("git" :: "commit" :: "-m" :: bigCommitMessage :: "pkgs/top-level/perl-packages.nix" :: Nil,
                  cwd = new java.io.File(repopath)).!
          bigCommitMessage = "perl packages update:\n\n"
        }
      }
      Process("git" :: "commit" :: "-m" :: bigCommitMessage :: "pkgs/top-level/perl-packages.nix" :: Nil,
              cwd = new java.io.File(repopath)).!
    }

  }
}
