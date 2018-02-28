#!/usr/bin/env perl
use File::Basename qw(dirname basename);
require("$ENV{HOME}/m/launcher/Launcher.pl.scala");

run(CLASS => 'CPANTester',
    TEE   => "cpantester.txt");
=cut
!#

import java.nio.file.{Paths, Files}
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


case class NixPerlPackage(author: Option[String], name: String, version: Version, url: String)

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
    case re2(a, n, v) => NixPerlPackage(Some(a), n.toUpperCase, v, url)
    case re3(   n, v) => NixPerlPackage(None   , n.toUpperCase, v, url)
  }
}


object NixPkgs {
  lazy val allPerlPackages: List[NixPerlPackage] = {
    val output = Process("nix-instantiate" :: "--show-trace" :: "--strict" :: "--eval" :: "-E" ::
                         """|with (import <nixpkgs> {});
                            |lib.concatMap (x: x.src.urls or []) (lib.attrValues perlPackages)
                            |""".stripMargin :: Nil,
                         None,
                         "NIXPKGS_CONFIG" -> "",
                         "NIX_PATH"       -> "nixpkgs=https://github.com/NixOS/nixpkgs/archive/master.tar.gz"
                         ).!!

    "\"([^ \"]+)\"".r.findAllMatchIn(output).map(_ group 1).toList flatMap { url =>
      Try(NixPerlPackage.fromString(url)) match {
        case Failure(_)  => System.err.println(s"ignore non-CPAN url $url"); None
        case Success(ca) =>                                                  Some(ca)
      }
    }
  }
}



case class CpanPerlPackage(author: String, name: String, version: Version)

object CpanOrg {
  lazy val allPerlPackages: List[CpanPerlPackage] = {
    if (!Files.exists(Paths get "02packages.details.txt")) {
      "wget https://raw.githubusercontent.com/metacpan/metacpan-cpan-extracted/master/02packages.details.txt".!
    }
    val re = (""".+ [A-Z]/[A-Z]{2}/([A-Z0-9-]+)(?:/[^/]+)*/([^/]+)-([^/-]+)""" + NixPerlPackage.extentions).r
    scala.io.Source.fromFile("02packages.details.txt").getLines.dropWhile(_.nonEmpty).dropWhile(_.isEmpty).flatMap{
      case re(a, n, v) =>                                      Some(CpanPerlPackage(a, n.toUpperCase, v))
      case line        => System.err.println(s"ignore $line"); None
    }.toList.distinct
  }
}

object CPANTester {
  def main(args: Array[String]) {
    //println(NixPkgs.allPerlPackages mkString "\n")
    //println(CpanOrg.allPerlPackages mkString "\n")
    val cpanByName:          Map[String,           List[CpanPerlPackage]] = CpanOrg.allPerlPackages groupBy (_.name)                      withDefaultValue Nil
    val cpanByAuthorAndName: Map[(String, String), List[CpanPerlPackage]] = CpanOrg.allPerlPackages groupBy (cpp => cpp.author->cpp.name) withDefaultValue Nil

    println(s"| nixpkgs url | problem |")
    println(s"| ---- | ---- |")

    for (NixPerlPackage(maybeauthor, name, version, url) <- NixPkgs.allPerlPackages) {
      if (!url.startsWith("mirror:"))
        println(s"| $url | the url does not use mirror:// scheme")
      maybeauthor match {
        case Some(author) =>
          cpanByAuthorAndName(author->name) match {
            case Nil       =>
              cpanByName(name) match {
                case Nil  =>
                  println(s"| $url | the package not found in CPAN")
                case cpps if cpps.exists(_.author != cpps.head.author) =>
                  println(s"| $url | the package not found in CPAN under author ${author} but now it has many authors ${cpps.map(_.author).distinct mkString ", "}; please specify one in nixpkgs")
                case cpps if cpps.exists(version < _.version) =>
                  println(s"| $url | the package not found in CPAN under author ${author} but ${cpps.head.author}; please update in nixpkgs if its correct")
                  println(s"|      | can be updated to ${cpps.map(_.version).filter(_ > version).sorted mkString ", "}")
                case cpps if cpps.forall(version != _.version) =>
                  println(s"| $url | the package not found in CPAN under author ${author} but ${cpps.head.author}; please update in nixpkgs if its correct")
                  println(s"|      | version $version used in nixpkgs not found in CPAN; there are only ${cpps.map(_.version).sorted mkString ", "}")
                case cpps =>
                  println(s"| $url | the package not found in CPAN under author ${author} but ${cpps.head.author}; please update in nixpkgs if its correct")
               }
            case cpps if cpps.exists(version < _.version) =>
              println(s"| $url | $version can be updated to ${cpps.map(_.version).filter(_ > version).sorted mkString ", "}")
            case cpps if cpps.forall(version != _.version) =>
              println(s"| $url | version $version used in nixpkgs not found in CPAN; there are only ${cpps.map(_.version).sorted mkString ", "}")
              cpanByName(name) match {
                case cpps if cpps.exists(version <= _.version) =>
                  println(s"|      | the package not found in CPAN also under authors ${cpps.map(_.author).distinct.sorted mkString ", "} with versions ${cpps.map(_.version).distinct.sorted mkString ", "}")
                case _ =>
              }
            case cpps =>
              //println(s"| $url | version $version is up to date")
          }
        case None =>
          cpanByName(name) match {
            case Nil       =>
              println(s"| $url | package $name not found in CPAN")
            case cpps if cpps.exists(_.author != cpps.head.author) =>
              println(s"| $url | package $name has many authors in CPAN ${cpps.map(_.author).distinct mkString ", "}; please specify one in nixpkgs")
            case cpps if cpps.exists(version < _.version) =>
              println(s"| $url | author ${cpps.head.author} not specified in nixpkgs")
              println(s"|      | can be updated to ${cpps.map(_.version).filter(_ > version).sorted mkString ", "}")
            case cpps if cpps.forall(version != _.version) =>
              println(s"| $url | author ${cpps.head.author} not specified in nixpkgs")
              println(s"|      | version $version used in nixpkgs not found in CPAN; there are only ${cpps.map(_.version).sorted mkString ", "}")
            case cpps =>
              //println(s"| $url | version $version is up to date")
          }
      }
    }

  }
}
