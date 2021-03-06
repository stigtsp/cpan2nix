#!/usr/bin/env perl
use File::Basename qw(dirname basename);
require("$ENV{HOME}/m/launcher/Launcher.pl.scala");

exit(run( CLASS     => 'Cpan2Nix'
#       , NOSERVER  => 1
#       , JAVA      => `nix-build --no-out-link '<nixpkgs-current>' -A oraclejdk14`  # custom JAVA_HOME
        , SCALA     => [ '2.12', '-Ywarn-unused-import' ]
        , TEE       => "cpan2nix.log"
    ));
=cut
!#

// TODO: remove (=null) modules which are included in perl

import `io.circe::circe-parser:0.9.1`
import `org.yaml:snakeyaml:1.20`
import `commons-io:2.6`,                            org.apache.commons.io.FileUtils

import ms.webmaster.launcher.position._
import java.io.File
import java.nio.file.{Paths, Files}
import scala.sys.process._
import scala.util.{Try, Failure, Success}
import scala.collection.JavaConverters._
import scala.Console._

import scala.concurrent.duration._
import scala.concurrent.Await
import `io.monix::monix:3.2.2`
import monix.eval.Task
import monix.execution.Scheduler

import `../_absalon/NixParser.scala`,               ms.webmaster.absalon.nixparser

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
    (a.vectorized.head, b.vectorized.head) match {
      case (INT(aint), INT(bint)) if aint != bint =>
        aint.toLong compare bint.toLong
      case _ =>
        (0 until l).prefixLength(i => a.vectorized(i) equals b.vectorized(i)) match {
          case `l` => a.vectorized.length compare b.vectorized.length
          case i   => a.vectorized(i) compare b.vectorized(i)
        }
    }
  }
  // perl versions usually do not follow semantic version rules, for example:
  require(Version("1.4201") < Version("1.45"))
  require(Version("1.07") < Version("1.3")) // perl specific
  require(Version("98.112902") < Version("2013.0523"))
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
  def fromString(s: String): Set[License] = s.stripPrefix("https://").stripPrefix("http://").stripPrefix("www.").stripSuffix(".php").stripSuffix(".html").stripSuffix(".txt").toLowerCase match {
    case null
       | "unknown"                                                       => Set.empty
    case "perl_5"       | "dev.perl.org/licenses/"
       | "perl"         | "opensource.org/licenses/perl"                 => Set(new License("artistic1"), new License("gpl1Plus"))
    case "open_source"                                                   => Set(new License("free"))
    case "gpl_1"        | "opensource.org/licenses/gpl-license"
       | "gpl"          | "gnu.org/licenses/gpl"                         => Set(new License("gpl1Plus"))
    case "gpl_2"        | "opensource.org/licenses/gpl-2.0"
       | "gpl2"         | "gnu.org/licenses/old-licenses/gpl-2.0"        => Set(new License("gpl2Plus"))
    case "lgpl_2_1"     | "gnu.org/licenses/lgpl-2.1"
       | "lgpl"         | "gnu.org/licenses/old-licenses/lgpl-2.1"       => Set(new License("lgpl21Plus"))
    case "lgpl_3_0"     | "opensource.org/licenses/lgpl-3.0"
       |                  "opensource.org/licenses/lgpl-license"         => Set(new License("lgpl3Plus"))
    case "gpl_3"        | "gnu.org/licenses/gpl-3.0"                     => Set(new License("gpl3Plus"))
    case "mozilla"      | "opensource.org/licenses/mozilla1.1"           => Set(new License("mpl11"))
    case "apache_2_0"                                                    => Set(new License("asl20"))
    case "artistic_1"   | "perlfoundation.org/artistic_license_1_0"
       | "artistic"     | "opensource.org/licenses/artistic-license"     => Set(new License("artistic1"))
    case "artistic_2"   | "opensource.org/licenses/artistic-license-2.0"
                        | "opensource.org/licenses/artistic-2.0"
                        | "perlfoundation.org/artistic-license-20"
                        | "perlfoundation.org/artistic_license_2_0"      => Set(new License("artistic2"))
    case "mit"          | "opensource.org/licenses/mit-license"          => Set(new License("mit"))
    case                  "wiki.creativecommons.org/wiki/public_domain"  => Set(new License("cc0"))
    case "unrestricted"                                                  => Set(new License("wtfpl"))
    case "freebsd"
       | "bsd"          | "opensource.org/licenses/bsd-license"
       |                  "opensource.org/licenses/bsd-3-clause"         => Set(new License("bsd3"))
    case x                                                               => //println(s"unknown license `$x'"); sys.exit(1)
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




case class NixPackage(maybeauthor: Option[Author], pname: Name, version: Version, url: String)

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
  def fromURL(url: String) = url match {
    case re2(a, pnameVersion) => val (n, v) = CpanErrata parseNameVersion pnameVersion; NixPackage(Some(Author(a)), n, v, url)
    case re3(   pnameVersion) => val (n, v) = CpanErrata parseNameVersion pnameVersion; NixPackage(None           , n, v, url)
  }
}


class NixPkgs(repopath: String /*File or URL*/) {
  // eval pkgs.perlPackages and extract all CPAN urls
  lazy val allPackages: List[NixPackage] = {

    // TODO: fallback to src.urls if there is no `pname`
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
      Try(NixPackage.fromURL(url)) match {
        case Failure(_)  => //System.err.println(s"ignore non-CPAN url $url")
                            None
        case Success(ca) => Some(ca)
      }
    }
  }
}




case class CpanPackage private(author: Author, pname: Name, version: Version, path: String) {
  lazy val tarballFile: File         = Cpan.downloadFile(this.path) getOrElse (throw new RuntimeException(s"${this.path} not found"))
  lazy val metaFile:    Option[File] = Cpan.downloadFile((NixPackage.extentions+"$").r.replaceAllIn(this.path, ".meta"  ))
//lazy val readmeFile:  Option[File] = Cpan.downloadFile((NixPackage.extentions+"$").r.replaceAllIn(this.path, ".readme"))
  lazy val sha256:      SHA256       = SHA256 fromString s"nix hash-file --type sha256 --base32 ${tarballFile}".!!.trim

  lazy val filesInTarball: Array[String]  = tarballFile.getName match {
                                              case name if name.endsWith(".tar.gz") || name.endsWith(".tar.bz2") || name.endsWith(".tgz")  => s"tar --list --warning=no-unknown-keyword --file $tarballFile".!! split '\n'
                                              case name if name.endsWith(".pm.gz")                                                         => Array(tarballFile.getName stripSuffix ".gz")
                                              case name if name.endsWith(".zip")                                                           => //s"bash -c 'T=$$(mktemp -d); mkdir -p $$T; 7z x -o$$T $tarballFile > /dev/null; cd $$T; find -type f'".!!.split('\n').map(_ stripPrefix "./")
                                                                                                                                              val zis = new java.util.zip.ZipInputStream(new java.io.FileInputStream(tarballFile))
                                                                                                                                              try {
                                                                                                                                                Iterator.continually(zis.getNextEntry).takeWhile(_ != null).filterNot(_.isDirectory).map(_.getName).toArray
                                                                                                                                              } catch {
                                                                                                                                                case e: java.util.zip.ZipException => println(RED+BOLD+e+RESET); Array()
                                                                                                                                              } finally {
                                                                                                                                                zis.close()
                                                                                                                                              }
                                            }
  lazy val isModule:       Boolean        = !pname.toString.equalsIgnoreCase("Module-Build") && filesInTarball.contains(s"$pname-$version/Build.PL")

  case class MetaExcerpt(runtimeMODs: Map[Mod, Version],
                         buildMODs:   Map[Mod, Version],
                         description: Option[String],
                         licenses:    Set[License],
                         homepage:    Option[String])

  lazy val meta: MetaExcerpt = {
    val metaContent:      String              = metaFile.fold("")(file => FileUtils.readFileToString(file, "UTF-8"))
    val fixedMetaContent: String              = (metaFile.fold("")(_.getName) match { // fix malformed yaml/json
                                                  case _ if metaContent.contains("\u0000")                           => "" // VMS format or other binary
                                                  case "String-CamelCase-0.03.meta"                                  => metaContent.replace("author:       author:"                                                  , "author:"                                                                )
                                                  case "Pangloss-0.06.meta"
                                                     | "Array-Stream-Transactional-Matcher-1.00.meta"
                                                     | "OpenFrame-WebApp-0.04.meta"
                                                     | "Acme-Scurvy-Whoreson-BilgeRat-Backend-insultserver-1.0.meta" => metaContent.replace("\n:\n"                                                                  , ":\n"                                                                    )
                                                  case "Template-Plugin-KwikiFormat-1.04.meta"                       => metaContent.replace("\nTanimoto"                                                             , " Tanimoto"                                                              )
                                                  case "Daemon-Whois-1.11.meta"                                      => metaContent.replace("\nunder the following license: Eclipse Public License, Version 1.0\nSee", " under the following license Eclipse Public License, Version 1.0 See"   )
                                                  case "Class-ArrayObjects-1.03.meta"                                => metaContent.replace("Originan author:"                                                       , "Originan author "                                                       )
                                                                                                                                   .replace("Manitained by:"                                                         , "Manitained by "                                                         )
                                                  case "List-Member-0.044-withoutworldwriteables.meta"               => metaContent.replace("PROLOG's member/2:"                                                     , "PROLOG's member/2 "                                                     )
                                                  case "Crypt-FileHandle-0.03.meta"                                  => metaContent.replace("\"runtime\" : \n"                                                       , "\"runtime\" : {\n"                                                      )
                                                  case "DBIx-JCL-0.12.meta"                                          => metaContent.replace("DBIx::JCL"                                                              , "DBIx::JCL:"                                                             )
                                                  case "Geo-Coordinates-KKJ-0.01.meta"                               => metaContent.replace("Geo::Coordinates::KKJ"                                                  , "Geo::Coordinates::KKJ:"                                                 )
                                                  case "MP3-Tag-1.13.meta"                                           => metaContent.replace("\"Thomas Geffert\" <thg@users.sourceforge.net>, \"Ilya Zakharevich\""   , "Thomas Geffert <thg@users.sourceforge.net>, Ilya Zakharevich"           )
                                                  case "Acme-AbhiIsNot-0.03.meta"                                    => metaContent.replace("\"abhishek\" <\"abhishekisnot@gmail.com\">"                             , "abhishek <abhishekisnot@gmail.com>"                                     )
                                                  case "Debug-LTrace-0.03.meta"                                      => metaContent.replace("\"koorchik\" <\"koorchik@cpan.org\">"                                   , "koorchik <koorchik@cpan.org>"                                           )
                                                  case "Data-Dmap-0.08.meta"                                         => metaContent.replace("\"Michael Zedeler\" <\"michael@zedeler.dk\">"                           , "Michael Zedeler <michael@zedeler.dk>"                                   )
                                                  case "MojoX-Run-0.15.meta"                                         => metaContent.replace("\"Brane F. Gracnar\" <\"bfg@frost.ath.cx\">"                            , "Brane F. Gracnar <bfg@frost.ath.cx>"                                    )
                                                  case "LibCAS-0.02a.meta"                                           => metaContent.replace("\"Michael Morris\" <\"michael.m.morris@gmail.com\">"                    , "Michael Morris <michael.m.morris@gmail.com>"                            )
                                                  case "PFIX-0.03.meta"                                              => metaContent.replace("\"Gabriel Galibourg\" <\"\">"                                           , "Gabriel Galibourg"                                                      )
                                                  case "VANAMBURG-Magic-0.03.meta"
                                                     | "VANAMBURG-Magic-0.04.meta"                                   => metaContent.replace("\"Gordon Van Amburg\" <\"vanamburg@cpan.org\">"                         , "Gordon Van Amburg <vanamburg@cpan.org>"                                 )
                                                  case "Document-eSign-Docusign-0.06.meta"                           => metaContent.replace("Tyler Hardison <tyler@seraph-net.net>, \n"                              , "Tyler Hardison <tyler@seraph-net.net>, "                                )
                                                  case "Data-FormValidator-Constraints-HTTP-0.01.meta"               => metaContent.replace("\nconstraint methods"                                                   , " constraint methods"                                                    )
                                                  case "Maypole-Plugin-Authentication-UserSessionCookie-1.8.meta"    => metaContent.replace("\nMaintained by Marcus Ramberg, C<marcus@thefeed.no>"                   , " Maintained by Marcus Ramberg, C<marcus@thefeed.no>"                    )
                                                                                                                                   .replace("\noptionally, users"                                                    , " optionally, users"                                                     )
                                                  case "ASP4-1.087.meta"                                             => metaContent.replace("Time::HiRes "                                                           , "Time::HiRes:"                                                           )
                                                  case "Pod-WikiText-0.12.meta"                                      => metaContent.replace("Pod::WikiText"                                                          , "Pod::WikiText:"                                                         )
                                                  case "Bryar-4.0.meta"                                              => metaContent.replace("Template::Provider::Encoding 0"                                         , "Template::Provider::Encoding: 0"                                        )
                                                  case "Apache2-WebApp-Plugin-CGI-0.10.meta"                         => metaContent.replace("Test::More"                                                             , "Test::More: 0"                                                          )
                                                  case "WWW-LinkedIn-0.004.meta"                                     => metaContent.replace("  Carp: 0"                                                              , "    Carp: 0"                                                            )
                                                  case "Prancer-Plugin-Log4perl-1.01.meta"                           => metaContent.replace("\"Try::Tiny\" : \"0\","                                                 , "\"Try::Tiny\" : \"0\""                                                  )
                                                  case "WWW-PagerDuty-0.2.meta"                                      => metaContent.replace("\"LWP::UserAgent\" : 0,"                                                , "\"LWP::UserAgent\" : 0"                                                 )
                                                  case "TAP-Formatter-BambooExtended-1.01.meta"                      => metaContent.replace("\"File::Slurp\" : \"0\","                                               , ",\"File::Slurp\" : \"0\","                                              )
                                                  case "Monitoring-Icinga2-Client-REST-2.0.4.meta"                   => metaContent.replace("\"JSON::PP version 2.97001\""                                           , "\"JSON::PP version 2.97001\"}"                                          )
                                                  case "URI-cpan-1.007.meta"                                         => metaContent.replace("URI::_generic"                                                          , "URI"                                                                    )
                                                  case "Data-Predicate-2.1.1.meta"                                   => metaContent.replace("- \"ayates\" <\"\">"                                                    , "- \"ayates <>\""                                                        )
                                                  case "Mojolicious-Command-migration-0.17.meta"
                                                     | "Mojolicious-Plugin-Sentry-0.11.meta"
                                                     | "Ubic-Watchdog-Notice-0.31.meta"                              => metaContent.replace("\\@"                                                                    , "@"                                                                      )
                                                  case "Maypole-View-Mason-0.3.meta"                                 => metaContent.replace("Simon Cozens\nMarcus Ramberg"                                           , "Simon Cozens, Marcus Ramberg"                                           )
                                                  case "MIME-Charset-1.011.3.meta"                                   => metaContent.replace("'"                                                                      , "\""                                                                     )
                                                  case "Net-Enum-DNSServer.meta"                                     => metaContent.replace("provides =>"                                                            , "\"provides\":"                                                          )
                                                                                                                                   .replace("file    =>"                                                             , "\"file\":"                                                              )
                                                                                                                                   .replace("version =>"                                                             , "\"version\":"                                                           )
                                                                                                                                   .replace("'"                                                                      , "\""                                                                     )
                                                                                                                                   .replace("=>"                                                                     , ":"                                                                      )
                                                                                                                                   .replace("27_02\","                                                               , "27_02\""                                                                )
                                                                                                                                   .replace("DNS.pm\",\n   },"                                                       , "DNS.pm\"}"                                                              )
                                                  case "SAS-TRX-0.07.meta"                                           => metaContent.replace("[Abstract] class, provides SAS transport (XPORT) format decoding"       , "Abstract class, provides SAS transport (XPORT) format decoding"         )
                                                  case "Rubyish-Attribute-1.2.meta"                                  => metaContent.replace("ruby-like accessor builder: attr_accessor, attr_writer and attr_reader" , "ruby-like accessor builder. attr_accessor, attr_writer and attr_reader" )
                                                  case "Froody-42.034.meta"                                          => metaContent.replace("\nbut if you're reporting bugs I<please> use the RT system mentioned above so\nwe",
                                                                                                                                            " but if you're reporting bugs I<please> use the RT system mentioned above so we" )
                                                  case "Module-Build-Kwalitee-0.24.meta"                             => metaContent.replace("\nMark Fowler <mark@twoshortplanks.com>,\nNorman Nunley <nnunley@fotango.com>,\nChia-liang Kao <clkao@clkao.org>,\net al.",
                                                                                                                                            " Mark Fowler <mark@twoshortplanks.com>, Norman Nunley <nnunley@fotango.com>, Chia-liang Kao <clkao@clkao.org>, et al.")
                                                  case "Data-FetchPath-0.02.meta"                                    => metaContent.replace("\"eval\"able paths to your complex data values"                         , "eval'able paths to your complex data values"                            )
                                                  case "Wiki-Toolkit-Plugin-Ping-0.03.meta"                          => metaContent.replace("\"ping\" various services when nodes are written"                       , "ping various services when nodes are written"                           )
                                                  case "Task-CPANAuthors-STRO-2009.1223.meta"                        => metaContent.replace("\"Task\" for installing modules maintained by STRO"                     , "Task for installing modules maintained by STRO"                         )
                                                  case "IRCDHelp-0.02.meta"                                          => metaContent.replace("version_from "                                                          , "version_from: "                                                         )
                                                                                                                                   .replace("\t\tdistribution_type: plugin"                                          , "distribution_type: plugin"                                              )
                                                  case "Oracle-ZFSSA-Client-0.01.meta"                               => metaContent.replace("\"LWP::UserAgent\" : \"6.05\""                                          , "\"LWP::UserAgent\" : \"6.05\","                                         )
                                                                                                                                   .replace("\"JSON\" : \"2.90\""                                                    , "\"JSON\" : \"2.90\","                                                   )
                                                  case "AlignAid-v0.0.2.meta"
                                                     | "Apache-Auth-Subrequest-v0.0.1.meta"
                                                     | "Apache-Image-v0.0.4.meta"
                                                     | "CGI-Wiki-Store-Mediawiki-0.02.meta"
                                                     | "Pushmi-v1.0.0.meta"
                                                     | "SVK-Log-Filter-Babelfish-0.0.3.meta"
                                                     | "SVK-Log-Filter-Date-0.0.1.meta"
                                                     | "SVK-Log-Filter-Mndrix-0.0.3.meta"
                                                     | "SVK-Log-Filter-Stats-0.0.4.meta"
                                                     | "SVK-v2.0.2.meta"
                                                     | "Text-CSV_PP-Simple-0.0.5.meta"
                                                     | "Time-Piece-Adaptive-0.03.meta"                               => metaContent.replaceAll("version: !.+\n"                                                      , "version:\n"                                                             )
                                                  case "Apache-SPARQL-0.22.meta"
                                                     | "Apache-SPARQL-RDFStore-0.3.meta"
                                                     | "Apache-Session-DBMS-0.32.meta"
                                                     | "Class-DBI-Plugin-FilterOnClick-1.2.meta"
                                                     | "Class-DBI-Plugin-Senna-0.01.meta"
                                                     | "Tie-Senna-0.02.meta"
                                                     | "WWW-Sucksub-Attila-0.06.meta"
                                                     | "WWW-Sucksub-Divxstation-0.01.meta"
                                                     | "WWW-Sucksub-Divxstation-0.04.meta"
                                                     | "WWW-Sucksub-Extratitles-0.01.meta"
                                                     | "WWW-Sucksub-Frigo-0.03.meta"
                                                     | "WWW-Sucksub-Vostfree-0.05.meta"
                                                     | "Net-Delicious-Export-1.2.meta"
                                                     | "Package-Relative-0.01.meta"
                                                     | "Test-TAP-HTMLMatrix-0.09.meta"
                                                     | "XML-Trivial-0.06.meta"
                                                     | "SQL-Interpolate-0.32.meta"
                                                     | "Net-Delicious-Export-Post-XBEL-1.4.meta"                     => metaContent.replaceAll(":\\s+>=?\\s*"                                                        , ": "                                                                     )
                                                  case "VUser-Install-0.1.0.meta"                                    => metaContent.replaceAll("@ echo\\s+"                                                          , "0\n    "                                                                )
                                                  case "XLS-Simple-0.02.meta"                                        => metaContent.replaceAll("[\u0080-\uffff]"                                                     , "  "                                                                     )
                                                  case "Video-FFmpeg-0.47.meta"                                      => metaContent split '\n' map (_.stripPrefix(" ")) mkString "\n"
                                                  case "Asm-Z80-Table-0.03.meta"
                                                     | "RandomJungle-0.05.meta"
                                                     | "DMTF-CIM-WSMan-v0.09.meta"                                   => metaContent split '\n' map (_.stripPrefix("{").stripSuffix("\r").stripSuffix("}").replace("{{", "{").replace("}}", "}")) mkString "\n"
                                                  case _                                                             => metaContent
                                                }).replaceAll("[\u0001-\u0008\u000B-\u001F]", "").replace("\t", "  ")
    var runtime:          Map[String, String] =                                                Map.empty
    var build:            Map[String, String] = if (isModule) Map("Module::Build" -> "0") else Map.empty
    var description:      Option[String]      = None
    var licenses:         Set[License]        = Set.empty
    var homepage:         Option[String]      = None

    if (fixedMetaContent startsWith "{") {
      val Right(json) = io.circe.parser.parse(fixedMetaContent)
      for (path   <- List( List("prereqs", "runtime", "requires") );
           m      <- path.foldLeft[io.circe.ACursor](json.hcursor)(_ downField _).as[Map[String,io.circe.Json]];
           (k, v) <- m) {
        runtime += k -> (v.asString orElse v.asNumber.map(_.toString) getOrElse ???)
      }
      for (path   <- List( List( "prereqs",     "configure", "requires")
                         , List( "prereqs",     "build",     "requires")
                         , List( "prereqs",     "test",      "requires")
                         , List( "prereqs",     "test",      "suggests")
                         , List( "x_alienfile", "requires",  "share"   )
                         );
           m      <- path.foldLeft[io.circe.ACursor](json.hcursor)(_ downField _).as[Map[String,io.circe.Json]];
           (k, v) <- m) {
        build += k -> (v.asString orElse v.asNumber.map(_.toString) getOrElse ???)
      }
      description = json.hcursor.downField("abstract").as[String].toOption
      licenses    = json.hcursor.downField("resources").downField("license").as[Set[String]].getOrElse(Set.empty).flatMap(License.fromString(_)) ++
                    json.hcursor.downField(                       "license").as[Set[String]].getOrElse(Set.empty).flatMap(License.fromString(_))
      homepage    = json.hcursor.downField("resources").downField("homepage").as[String].toOption
    } else if (fixedMetaContent.nonEmpty) {
      val yaml: java.util.Map[String, Any] = new org.yaml.snakeyaml.Yaml load fixedMetaContent
      require(yaml != null, s"invalid yaml $path")

      yaml.get("requires"          ) match { case null                  =>
                                             case s: String             => //println(__LINE__, s"s=[$s]")
                                                                           runtime ++= s.trim.split("\\s+").sliding(2,2).map{ case Array(k)   => k->"0"
                                                                                                                              case Array(k,v) => k->v }
                                             case m: java.util.Map[_,_] => runtime ++= m.asScala.map{ case (k,v) => k.toString->Option(v).fold("")(_.toString) } }
      yaml.get("configure_requires") match { case null                  =>
                                             case m: java.util.Map[_,_] => build   ++= m.asScala.map{ case (k,v) => k.toString->Option(v).fold("")(_.toString) } }
      yaml.get("build_requires"    ) match { case null                  =>
                                             case s: String             => //println(__LINE__, s"s=[$s]")
                                                                           build   ++= s.trim.split("\\s+").sliding(2,2).map{ case Array(k)   => k->"0"
                                                                                                                              case Array(k,v) => k->v }
                                             case m: java.util.Map[_,_] => build   ++= m.asScala.map{ case (k,v) => k.toString->Option(v).fold("")(_.toString) } }
      yaml.get("x_test_requires"   ) match { case null                  =>
                                             case m: java.util.Map[_,_] => build   ++= m.asScala.map{ case (k,v) => k.toString->Option(v).fold("")(_.toString) } }

      description = yaml.get("abstract") match {
                      case null => None
                      case s    => Some(s.toString)
                    }
      licenses    = (  (yaml.get("license") match {
                          case null                      => Set()
                          case s: String                 => Set(s)
                          case a: java.util.ArrayList[_] => a.asScala.map(_.toString).toSet
                        }) ++
                       (for (a <- Option(yaml.get("resources").asInstanceOf[java.util.Map[String, String]]);
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

  def suggestedNixpkgsName: String = pname.toString.replace("-", "").replace("+", "").replace("_", "")
}


object CpanPackage {
  private[this] val re       = ("""[A-Z]/[A-Z]{2}/([A-Z0-9-]+)(?:/[^/]+)*/([^/]+)""" + NixPackage.extentions).r
  private[this] val internat = new collection.mutable.HashMap[String, CpanPackage]
  def fromPath(path: String) = internat.getOrElseUpdate(path, path match {
    case re(author, nameVersion) => val (name, version) = CpanErrata parseNameVersion nameVersion
                                    CpanPackage(Author(author), name, version, path)
  })
}


object CpanErrata {
  // *** difficult parse cases?
  private[this] val exception1  = "(PerlMagick|triceps|WWW-Authenticate|Amazon-API|Amazon-Credentials|App-DDFlare|UnixODBC|Tk-Browser|Convert-Ethiopic|CPAN-WAIT|Daemonise|downsexta|DMAMisc|Document|Zobel|Fault|FileHash|Games-CroqueMonster|Geo-Coder-Many|LEGO-NXT|Math-GoldenBigMath|Math-MatrixReal-Ext1|MegaDistro|pfacter|Scanner|SOAP-MIME|Thrift)-([0-9.]+-[0-9.]+)".r
  private[this] val suspicious1 = "([^/]+)-[0-9.]+-([0-9.]+)".r
  private[this] val rule1       = "([^/]+)-([^/-]+)".r
  private[this] val badversion1 = "([A-Z][^/_-]*)_([^/-]+)".r // fix for strings like "App-NetdiscoX-Web-Plugin-GraphLinkSwitch_0.1" or "PDF-EasyPDF_0_04"
  private[this] val rule2       = "([^/-]+)_([^_/-]+)".r
  private[this] val badversion2 = "([a-zA-Z]+)".r             // fix for strings "Ipv4_networks" or "article_fetcher"
  private[this] val rule3       = "([^_/-]+)\\.([^\\._/-]+)".r
  private[this] val rule4       = "([^\\._/-]+)".r
//private[this] val rule3       = "([^_/-]+)".r
  private[this] val nvcache = collection.mutable.Map.empty[String, (Name, Version)] // to avoid flooding same warnings
  def parseNameVersion(s: String): (Name, Version) = nvcache.getOrElseUpdate(s, {
    val (n, v) = s match {
      case "Class-CompiledC2.21"              => (Name("Class-CompiledC"),                 Version("2.21"))
      case "Spreadsheet-WriteExcel-WebPivot2" => (Name("Spreadsheet-WriteExcel-WebPivot"), Version("2"))
      case "Devel-PPPort-3.48-again"          => (Name("Devel-PPPort"),                    Version("3.48-again"))
      case exception1 (n, v)                  => (Name(n),                                 Version(v))
      case suspicious1(n, v)                  => System.err.println(s"version of `$s` might be detected incorrectly as `$v`")
                                                 (Name(n),                                 Version(v))
      case rule1      (n, badversion1(n2, v)) => (Name(n+"-"+n2),                          Version(v))
      case rule1      (n, badversion2(n2))    => (Name(n+"-"+n2),                          Version(""))
      case rule1      (n, v)                  => (Name(n),                                 Version(v))
      case rule2      (n, badversion2(n2))    => (Name(n+"_"+n2),                          Version(""))
      case rule2      (n, v)                  => (Name(n),                                 Version(v))
      case rule3      (n, v)                  => (Name(n),                                 Version(v))
      case rule4      (n)                     => (Name(n),                                 Version(""))
    }
//  println(f"$s%-60s -> $n%-60s $v")
    (n, v)
  })

  // *** modules and packages to ignore
  val modsToIgnore             = Map( Mod("Catalyst::Engine::CGI")                       -> ((_:Version) => true)  // <- these are for old Catalyst-Runtime-5.8xxxx
                                    , Mod("Catalyst::Engine::FastCGI")                   -> ((_:Version) => true)
                                    , Mod("Catalyst::Engine::HTTP")                      -> ((_:Version) => true)
                                    , Mod("Catalyst::Engine::HTTP::Restarter")           -> ((_:Version) => true)
                                    , Mod("Catalyst::Engine::HTTP::Restarter::Watcher")  -> ((_:Version) => true)
                                    )
  val namesToIgnore            = Map( Name("Sys-Virt")                                   -> ((_:Version) => true) // must be updated in-sync with `pkgs.libvirt'
                                    , Name("Mac-SystemDirectory")                        -> ((_:Version) => true) // fails on linux, I cannot test
                                    , Name("Mac-Pasteboard")                             -> ((_:Version) => true) // fails on linux, I cannot test
                                    , Name("Regexp-Copy")                                -> ((_:Version) => true) // broken
                                    , Name("Catalyst-Engine-HTTP-Prefork")               -> ((_:Version) => true) // meta.broken = true
                                    , Name("Catalyst-Plugin-HTML-Widget")                -> ((_:Version) => true) // meta.broken = true
                                    , Name("Devel-SizeMe")                               -> ((_:Version) => true) // meta.broken = true
                                    , Name("Unicode-ICU-Collator")                       -> ((_:Version) => true) // meta.broken = true
                                    , Name("Mail-SPF")                                   -> ((_:Version) => true) // custom 'buildPhase'
                                    )

  // *** hack to work with packages wich are out of perl-packages.nix
  val inExternalNixFiles       = Set( Name("Compress-Raw-Zlib")                       // in an external file (todo? move into perl-packages.nix)
                                    , Name("DBD-SQLite")
                                    , Name("DBD-Pg")
                                    , Name("DBD-mysql")
                                    , Name("DB_File")
                                    )

  // *** do not add to nixpkgs dependencies present on cpan
  val dependenciesToBreak      = Map( Name("ack"                              ) -> Set( Name("Test-Harness"))               // complex buildInputs expression, need to update manually
                                    , Name("MooX-Options"                     ) -> Set( Name("MooX-ConfigFromFile"))        // circular dependency
                                    , Name("Plack"                            ) -> Set( Name("CGI-Compile"))                // to disable failing test
                                    , Name("Test-CleanNamespaces"             ) -> Set( Name("Moose")
                                                                                      , Name("MooseX-Role-Parameterized"))  // circular dependency
                                    , Name("Tie-Hash-Indexed"                 ) -> Set( Name("Test"))                       // wrong test framework?
                                    , Name("libapreq2"                        ) -> Set( Name("mod_perl"))                   // https://github.com/NixOS/nixpkgs/pull/59861
                                    , Name("Apache-Test"                      ) -> Set( Name("Win32-Process"))              // no Win32
                                    , Name("Device-MAC"                       ) -> Set( Name("Test-Simple"))
                                    ) withDefaultValue Set.empty


  // *** add to nixpkgs dependencies missing on cpan (usually due to missing .meta file; FIXME: look into Makefile.PL then)
  val extraBuildDependencies   = Map( Name("Alien-GMP"                               ) -> Map( Mod("Devel::CheckLib")                  -> Version("0"))
                                    , Name("Autodia"                                 ) -> Map( Mod("DBI")                              -> Version("0"))
                                    , Name("Array-FIFO"                              ) -> Map( Mod("Test::Trap")                       -> Version("0")
                                                                                             , Mod("Test::Deep::NoTest")               -> Version("0"))
                                    , Name("Archive-Zip"                             ) -> Map( Mod("Test::MockModule")                 -> Version("0"))
                                    , Name("Catalyst-Controller-POD"                 ) -> Map( Mod("inc::Module::Install")             -> Version("0"))
                                    , Name("Catalyst-Runtime"                        ) -> Map( Mod("Type::Tiny")                       -> Version("0"))
                                    , Name("Catalyst-Authentication-Store-DBIx-Class") -> Map( Mod("Test::Warn")                       -> Version("0"))
                                    , Name("Catalyst-Authentication-Store-Htpasswd"  ) -> Map( Mod("Test::WWW::Mechanize")             -> Version("0")
                                                                                             , Mod("Test::LongString")                 -> Version("0"))
                                    , Name("Catalyst-Controller-HTML-FormFu"         ) -> Map( Mod("Test::LongString")                 -> Version("0"))
                                    , Name("Catalyst-Controller-POD"                 ) -> Map( Mod("Test::WWW::Mechanize")             -> Version("0")
                                                                                             , Mod("Test::LongString")                 -> Version("0")
                                                                                             , Mod("inc::Module::Install")             -> Version("0"))
                                    , Name("Catalyst-Plugin-Cache"                   ) -> Map( Mod("Class::Accessor")                  -> Version("0"))
                                    , Name("Catalyst-Plugin-Cache-HTTP"              ) -> Map( Mod("Test::WWW::Mechanize")             -> Version("0")
                                                                                             , Mod("Test::LongString")                 -> Version("0"))
                                    , Name("Catalyst-View-Download"                  ) -> Map( Mod("Test::WWW::Mechanize")             -> Version("0")
                                                                                             , Mod("Test::LongString")                 -> Version("0"))
                                    , Name("Code-TidyAll"                            ) -> Map( Mod("Test::Class")                      -> Version("0")
                                                                                             , Mod("Test::Deep")                       -> Version("0")
                                                                                             , Mod("Test::Exception")                  -> Version("0")
                                                                                             , Mod("Test::Most")                       -> Version("0")
                                                                                             , Mod("Test::Warn")                       -> Version("0"))
                                    , Name("Corona"                                  ) -> Map( Mod("Test::SharedFork")                 -> Version("0")
                                                                                             , Mod("Test::TCP")                        -> Version("0"))
                                    , Name("CPAN"                                    ) -> Map( Mod("Archive::Zip")                     -> Version("0"))
                                    , Name("Crypt-SSLeay"                            ) -> Map( Mod("Path::Class")                      -> Version("0"))
                                    , Name("Data-FormValidator"                      ) -> Map( Mod("CGI")                              -> Version("0"))
                                    , Name("Data-Page-Pageset"                       ) -> Map( Mod("Class::Accessor")                  -> Version("0")
                                                                                             , Mod("Data::Page")                       -> Version("0")
                                                                                             , Mod("Test::Exception")                  -> Version("0"))
                                    , Name("Data-Taxi"                               ) -> Map( Mod("Debug::ShowStuff")                 -> Version("0"))
                                    , Name("DateTime-Calendar-Julian"                ) -> Map( Mod("DateTime")                         -> Version("0"))
                                    , Name("DBIx-Introspector"                       ) -> Map( Mod("Test::Fatal")                      -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-CheckChangeLog"        ) -> Map( Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-ReadmeAnyFromPod"      ) -> Map( Mod("Test::SharedFork")                 -> Version("0")
                                                                                             , Mod("Test::Differences")                -> Version("0")
                                                                                             , Mod("Test::Exception")                  -> Version("0")
                                                                                             , Mod("Test::Warn")                       -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-ReadmeMarkdownFromPod" ) -> Map( Mod("Test::Deep")                       -> Version("0")
                                                                                             , Mod("Test::Differences")                -> Version("0")
                                                                                             , Mod("Test::Exception")                  -> Version("0")
                                                                                             , Mod("Test::Warn")                       -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-CPAN-Changes"     ) -> Map( Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-CPAN-Meta-JSON"   ) -> Map( Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-DistManifest"     ) -> Map( Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-MinimumVersion"   ) -> Map( Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-Perl-Critic"      ) -> Map( Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-Synopsis"         ) -> Map( Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-UnusedVars"       ) -> Map( Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Dist-Zilla-Plugin-Test-Version"          ) -> Map( Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Font-TTF"                                ) -> Map( Mod("IO::String")                       -> Version("0"))
                                    , Name("FormValidator-Simple"                    ) -> Map( Mod("CGI")                              -> Version("0"))
                                    , Name("Gnome2-Canvas"                           ) -> Map( Mod("ExtUtils::Depends")                -> Version("0"))
                                    , Name("Gnome2-Canvas"                           ) -> Map( Mod("ExtUtils::PkgConfig")              -> Version("0")
                                                                                             , Mod("ExtUtils::Depends")                -> Version("0"))
                                    , Name("Gtk2-TrayIcon"                           ) -> Map( Mod("ExtUtils::PkgConfig")              -> Version("0")
                                                                                             , Mod("ExtUtils::Depends")                -> Version("0")
                                                                                             , Mod("Glib::CodeGen")                    -> Version("0"))
                                    , Name("Gtk2-Unique"                             ) -> Map( Mod("Glib::CodeGen")                    -> Version("0"))
                                    , Name("grepmail"                                ) -> Map( Mod("File::HomeDir::Unix")              -> Version("0"))
                                    , Name("Hash-Merge-Simple"                       ) -> Map( Mod("Test::Deep")                       -> Version("0")
                                                                                             , Mod("Test::Warn")                       -> Version("0")
                                                                                             , Mod("Test::Exception")                  -> Version("0")
                                                                                             , Mod("Test::Differences")                -> Version("0"))
                                    , Name("HTML-Selector-XPath"                     ) -> Map( Mod("Test::Base")                       -> Version("0"))
                                    , Name("HTML-Tidy"                               ) -> Map( Mod("Test::Exception")                  -> Version("0"))
                                    , Name("HTTP-Response-Encoding"                  ) -> Map( Mod("LWP::UserAgent")                   -> Version("0"))
                                    , Name("IO-Socket-Timeout"                       ) -> Map( Mod("Test::SharedFork")                 -> Version("0"))
                                    , Name("JSON"                                    ) -> Map( Mod("Test::Pod")                        -> Version("0")) // for optional test, oxij added in https://github.com/NixOS/nixpkgs/pull/44739
                                    , Name("Mail-Mbox-MessageParser"                 ) -> Map( Mod("File::Slurper")                    -> Version("0")
                                                                                             , Mod("Test::Pod")                        -> Version("0")
                                                                                             , Mod("Test::Pod::Coverage")              -> Version("0"))
                                    , Name("Module-Build-Pluggable-PPPort"           ) -> Map( Mod("Test::SharedFork")                 -> Version("0"))
                                    , Name("Module-Info"                             ) -> Map( Mod("Test::Pod")                        -> Version("0")
                                                                                             , Mod("Test::Pod::Coverage")              -> Version("0"))
                                    , Name("MooseX-Has-Options"                      ) -> Map( Mod("Test::Deep")                       -> Version("0")
                                                                                             , Mod("Test::Differences")                -> Version("0")
                                                                                             , Mod("Test::Exception")                  -> Version("0")
                                                                                             , Mod("Test::Warn")                       -> Version("0"))
                                    , Name("Net-SCP"                                 ) -> Map( Mod("String::ShellQuote")               -> Version("0")
                                                                                             , Mod("Net::SSH")                         -> Version("0"))
                                    , Name("PerlIO-via-symlink"                      ) -> Map( Mod("inc::Module::Install")             -> Version("0"))
                                    , Name("PerlIO-via-Timeout"                      ) -> Map( Mod("Test::SharedFork")                 -> Version("0"))
                                    , Name("Plack"                                   ) -> Map( Mod("Test::SharedFork")                 -> Version("0"))
                                    , Name("Plack-App-Proxy"                         ) -> Map( Mod("Test::SharedFork")                 -> Version("0"))
                                    , Name("Plack-Test-ExternalServer"               ) -> Map( Mod("Test::SharedFork")                 -> Version("0"))
                                    , Name("Plack-Middleware-Auth-Digest"            ) -> Map( Mod("Test::SharedFork")                 -> Version("0")
                                                                                             , Mod("Test::TCP")                        -> Version("0"))
                                    , Name("Plack-Middleware-Deflater"               ) -> Map( Mod("Test::SharedFork")                 -> Version("0")
                                                                                             , Mod("Test::TCP")                        -> Version("0"))
                                    , Name("Plack-Middleware-Session"                ) -> Map( Mod("Test::SharedFork")                 -> Version("0")
                                                                                             , Mod("Test::TCP")                        -> Version("0"))
                                    , Name("Protocol-HTTP2"                          ) -> Map( Mod("Test::SharedFork")                 -> Version("0"))
                                    , Name("REST-Utils"                              ) -> Map( Mod("Test::LongString")                 -> Version("0")
                                                                                             , Mod("Test::WWW::Mechanize")             -> Version("0"))
                                    , Name("RT-Client-REST"                          ) -> Map( Mod("CGI")                              -> Version("0")
                                                                                             , Mod("DateTime")                         -> Version("0")
                                                                                             , Mod("DateTime::Format::DateParse")      -> Version("0")
                                                                                             , Mod("Error")                            -> Version("0")
                                                                                             , Mod("Exception::Class")                 -> Version("0")
                                                                                             , Mod("HTTP::Cookies")                    -> Version("0")
                                                                                             , Mod("LWP::UserAgent")                   -> Version("0")
                                                                                             , Mod("Params::Validate")                 -> Version("0")
                                                                                             , Mod("Test::Exception")                  -> Version("0"))
                                    , Name("Starlet"                                 ) -> Map( Mod("Test::SharedFork")                 -> Version("0"))
                                    , Name("Task-Plack"                              ) -> Map( Mod("Test::SharedFork")                 -> Version("0"))
                                    , Name("Task-FreecellSolver-Testing"             ) -> Map( Mod("Test::Trap")                       -> Version("0"))
                                    , Name("Term-ProgressBar-Simple"                 ) -> Map( Mod("Test::MockObject")                 -> Version("0"))
                                    , Name("Test-Class-Most"                         ) -> Map( Mod("Test::Differences")                -> Version("0")
                                                                                             , Mod("Test::Deep")                       -> Version("0")
                                                                                             , Mod("Test::Exception")                  -> Version("0")
                                                                                             , Mod("Test::Warn")                       -> Version("0"))
                                    , Name("Test-Run-Plugin-ColorFileVerdicts"       ) -> Map( Mod("Test::Trap")                       -> Version("0"))
                                    , Name("Test-Run-Plugin-ColorSummary"            ) -> Map( Mod("Test::Trap")                       -> Version("0"))
                                    , Name("Test-WWW-Mechanize"                      ) -> Map( Mod("Test::LongString")                 -> Version("0"))
                                    , Name("Test-WWW-Mechanize-CGI"                  ) -> Map( Mod("Test::LongString")                 -> Version("0"))
                                    , Name("Test-WWW-Mechanize-PSGI"                 ) -> Map( Mod("Test::LongString")                 -> Version("0"))
                                    , Name("Twiggy"                                  ) -> Map( Mod("Test::SharedFork")                 -> Version("0"))
                                    , Name("Cache-KyotoTycoon"                       ) -> Map( Mod("Test::SharedFork")                 -> Version("0"))
                                    , Name("YAML"                                    ) -> Map( Mod("Test::Base")                       -> Version("0"))
                                    , Name("Net-IP-Lite"                             ) -> Map( Mod("Test::Exception")                  -> Version("0"))
                                    , Name("Mojo-Pg"                                 ) -> Map( Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Mojo-mysql"                              ) -> Map( Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Sereal"                                  ) -> Map( Mod("Test::Deep")                       -> Version("0")
                                                                                             , Mod("Test::MemoryGrowth")               -> Version("0"))
                                    , Name("LWP-UserAgent-DNS-Hosts"                 ) -> Map( Mod("Test::TCP")                        -> Version("0")
                                                                                             , Mod("Test::SharedFork")                 -> Version("0"))
                                    , Name("Net-FreeDB"                              ) -> Map( Mod("Test::Most")                       -> Version("0")
                                                                                             , Mod("Test::Exception")                  -> Version("0")
                                                                                             , Mod("Test::Differences")                -> Version("0")
                                                                                             , Mod("Test::Warn")                       -> Version("0")
                                                                                             , Mod("Test::Deep")                       -> Version("0"))
                                    , Name("SDL"                                     ) -> Map( Mod("Test::Exception")                  -> Version("0")
                                                                                             , Mod("Test::Differences")                -> Version("0")
                                                                                             , Mod("Test::Warn")                       -> Version("0")
                                                                                             , Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Device-MAC"                              ) -> Map( Mod("Test::Exception")                  -> Version("0")
                                                                                             , Mod("Test::Differences")                -> Version("0")
                                                                                             , Mod("Test::Warn")                       -> Version("0")
                                                                                             , Mod("Test::Deep")                       -> Version("0"))
                                    , Name("Device-OUI"                              ) -> Map( Mod("Test::Exception")                  -> Version("0"))

                                    ) withDefaultValue Map.empty
  val extraRuntimeDependencies = Map( Name("Alien-Build"                      ) -> Map( Mod("PkgConfig")                    -> Version("0"))
                                    , Name("Any-Moose"                        ) -> Map( Mod("Mouse")                        -> Version("0")
                                                                                      , Mod("Moose")                        -> Version("0"))
                                    , Name("Crypt-PKCS10"                     ) -> Map( Mod("Convert::ASN1")                -> Version("0"))
                                    , Name("Crypt-SSLeay"                     ) -> Map( Mod("LWP::Protocol::https")         -> Version("0")
                                                                                      , Mod("Bytes::Random::Secure")        -> Version("0"))
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
                                    , Name("Net-SSH-Perl"                     ) -> Map( Mod("File::HomeDir")                -> Version("0"))
                                    , Name("Proc-WaitStat"                    ) -> Map( Mod("IPC::Signal")                  -> Version("0"))
                                    , Name("RSS-Parser-Lite"                  ) -> Map( Mod("local::lib")                   -> Version("0"))
                                    , Name("Statistics-TTest"                 ) -> Map( Mod("Statistics::Distributions")    -> Version("0")
                                                                                      , Mod("Statistics::Descriptive")      -> Version("0"))
                                    , Name("GoferTransport-http"              ) -> Map( Mod("mod_perl2")                    -> Version("0"))
                                    , Name("Text-WrapI18N"                    ) -> Map( Mod("Text::CharWidth")              -> Version("0"))
                                    , Name("Text-SimpleTable"                 ) -> Map( Mod("Unicode::GCString")            -> Version("0")) // or Text::VisualWidth::UTF8 or Text::VisualWidth::PP
                                    , Name("XML-SAX"                          ) -> Map( Mod("XML::SAX::Exception")          -> Version("0"))
                                    , Name("XML-Grove"                        ) -> Map( Mod("Data::Grove")                  -> Version("0"))
                                    , Name("XML-Handler-YAWriter"             ) -> Map( Mod("XML::Parser::PerlSAX")         -> Version("0"))
                                    , Name("ExtUtils-F77"                     ) -> Map( Mod("File::Which")                  -> Version("0"))
                                    , Name("CPANPLUS"                         ) -> Map( Mod("Archive::Extract")             -> Version("0")  // https://github.com/NixOS/nixpkgs/pull/41394#issuecomment-394208166
                                                                                      , Mod("Log::Message")                 -> Version("0")
                                                                                      , Mod("Module::Pluggable")            -> Version("0")
                                                                                      , Mod("Object::Accessor")             -> Version("0")
                                                                                      , Mod("Package::Constants")           -> Version("0")
                                                                                      , Mod("Term::UI")                     -> Version("0"))
                                    , Name("JSON-Validator"                   ) -> Map( Mod("Data::Validate::Domain")       -> Version("0") // https://github.com/NixOS/nixpkgs/pull/70335#issuecomment-538054983
                                                                                      , Mod("Data::Validate::IP")           -> Version("0")
                                                                                      , Mod("Net::IDN::Encode")             -> Version("0")
                                                                                      , Mod("YAML::XS")                     -> Version("0"))
                                    , Name("Crypt-ScryptKDF"                  ) -> Map( Mod("Crypt::OpenSSL::Random")       -> Version("0")) // https://github.com/NixOS/nixpkgs/pull/71128
                                    , Name("DBD-Sybase"                       ) -> Map( Mod("DBI")                          -> Version("0"))
                                    , Name("CatalystX-Script-Server-Starman"  ) -> Map( Mod("MooseX::Types")                -> Version("0")
                                                                                      , Mod("Pod::Parser")                  -> Version("0"))
                                    , Name("Device-OUI"                       ) -> Map( Mod("Class::Accessor::Grouped")     -> Version("0")
                                                                                      , Mod("Sub::Exporter")                -> Version("0")
                                                                                      , Mod("LWP")                          -> Version("0"))
                                    , Name("Crypt-DES_EDE3"                   ) -> Map( Mod("Crypt::DES")                   -> Version("0")) // .meta file is 404
                                    , Name("CPAN-Mini"                        ) -> Map( Mod("LWP::Protocol::https")         -> Version("0")) // https://github.com/NixOS/nixpkgs/pull/97098#pullrequestreview-484545271
                                    ) withDefaultValue Map.empty

  // *** pinned packages
  val pinnedPackages           = Set( CpanPackage fromPath "N/NJ/NJH/MusicBrainz-DiscID-0.03.tar.gz"                 // need to review patchPhase manually
                                    , CpanPackage fromPath "M/MS/MSISK/HTML-TableExtract-2.13.tar.gz"                // 2.15 seems broken
                                    , CpanPackage fromPath "R/RR/RRA/podlators-4.10.tar.gz"                          // 4.11,4.12,4.14 test failed
                                    , CpanPackage fromPath "L/LD/LDS/VM-EC2-1.28.tar.gz"                             // prevent downgrade to 1.25
                                    , CpanPackage fromPath "G/GU/GUIDO/libintl-perl-1.31.tar.gz"                     // AppSqitch tries to downgrade to 1.30
//                                  , CpanPackage fromPath "T/TI/TINITA/Inline-0.83.tar.gz"                          // prevent downgrade to 0.82
                                    , CpanPackage fromPath "N/NA/NANIS/Crypt-SSLeay-0.73_06.tar.gz"                  // newer than in CPAN
//                                  , CpanPackage fromPath "I/IS/ISAAC/libapreq2-2.13.tar.gz"                        // error parsing derivation (span2nix fixes sha256 of a patch)
                                    , CpanPackage fromPath "G/GA/GAAS/HTTP-Daemon-6.01.tar.gz"                       // newer version depends on Module::Build which fails to cross-compile
                                    , CpanPackage fromPath "R/RU/RURBAN/Cpanel-JSON-XS-4.17.tar.gz"                  // 4.21 add many new deps which do fail
                                    , CpanPackage fromPath "S/SH/SHANCOCK/Perl-Tidy-20200110.tar.gz"                 // 20200822 test fails
                                    , CpanPackage fromPath "R/RI/RIBASUSHI/DBIx-Class-0.082841.tar.gz"               // 0.082842 breaks Catalyst-Authentication-Store-DBIx-Class
                                    )

  // *** enforce 'doCheck = false' or 'doCheck = false'
  val doCheckOverride          = Map( Name("Net-HTTP")                                  -> (false, "wants network")
                                    , Name("Net-Amazon-MechanicalTurk")                 -> (false, "wants network")
                                    , Name("Task-Catalyst-Tutorial")                    -> (false, "fails with 'open3: exec of .. perl .. failed: Argument list too long at .../TAP/Parser/Iterator/Process.pm line 165.'")
                                    , Name("Dist-Zilla-PluginBundle-TestingMania")      -> (false, "fails with 'open3: exec of .. perl .. failed: Argument list too long at .../TAP/Parser/Iterator/Process.pm line 165.'")
                                    , Name("Catalyst-Controller-HTML-FormFu")           -> (false, "fails with 'open3: exec of .. perl .. failed: Argument list too long at .../TAP/Parser/Iterator/Process.pm line 165.'")
                                    , Name("RSS-Parser-Lite")                           -> (false, "creates files in HOME")
                                    , Name("B-C")                                       -> (false, "test fails")
                                    , Name("Test-Cmd")                                  -> (false, "test fails")
                                    , Name("Crypt-OpenPGP")                             -> (false, "test fails with 'No random source available!'")
                                    )
}



object Cpan {
          val byMod           = new collection.mutable.HashMap[Mod,            Set[CpanPackage]] withDefaultValue Set.empty
          val byName          = new collection.mutable.HashMap[Name,           Set[CpanPackage]] withDefaultValue Set.empty
          val byAuthorAndName = new collection.mutable.HashMap[(Author, Name), Set[CpanPackage]] withDefaultValue Set.empty
          val providedMods    = new collection.mutable.HashMap[CpanPackage,    Map[Mod,Version]] withDefaultValue Map.empty

  locally {
    if (!new File("02packages.details.txt").exists) {
      "wget https://raw.githubusercontent.com/metacpan/metacpan-cpan-extracted/master/02packages.details.txt".!
    }
    val re = ("""(\S+)\s+(\S+)\s+([A-Z]/[A-Z]{2}/[A-Z0-9-]+(?:/[^/]+)*/[^/]+""" + NixPackage.extentions + ")").r
    scala.io.Source.fromFile("02packages.details.txt").getLines dropWhile(_.nonEmpty) dropWhile(_.isEmpty) foreach {
      case re(modstr, modverstr, path) => Try(CpanPackage fromPath path) match {
                                            case Success(cp) =>
                                              val mod        = Mod(modstr)
                                              byMod          (mod                ) += cp
                                              byName         (           cp.pname) += cp
                                              byAuthorAndName(cp.author->cp.pname) += cp
                                              providedMods   (cp                 ) += mod -> Version(modverstr)
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


class PerlDerivation(repopath: File, name: String /* = "perl530"*/, val perlVersion: String /* = "5.30.3"*/) {
  private[this] var derivation = Process( "nix-build" :: "--show-trace"
                                       :: "--option" :: "binary-caches" :: "http://cache.nixos.org/"
                                       :: ( Cpan2Nix.builder_X86_64 match {
                                              case Worker(system, concurrency, Worker.Remote(user, host, sshopts)) =>
                                                ( "--option" :: "builders-use-substitutes" :: "true"
                                               :: "-j0"
                                               :: "--builders" :: s"ssh://${user}@${host} ${system} /home/user/.ssh/id_ed25519 ${concurrency} ${concurrency} kvm,big-parallel,gccarch-sandybridge,gccarch-skylake"
                                               :: Nil
                                                )
                                              case Worker(system, concurrency, Worker.Local) =>
                                                Nil
                                           })
                                      ::: "-E" :: s"(import <nixpkgs> { }).$name" :: Nil,
                                          cwd = repopath,
                                          "NIXPKGS_CONFIG" -> "",
                                          "NIX_PATH"       -> s"nixpkgs=${repopath.getAbsolutePath}",
                                          "NIX_SSHOPTS"    -> (Cpan2Nix.builder_X86_64 match {
                                                                case Worker(system, concurrency, Worker.Remote(user, host, sshopts)) => sshopts.mkString(" ")
                                                                case Worker(system, concurrency, Worker.Local)                       => ""
                                                              })
                                        ).!!.trim

  private[this] val localVersions = collection.mutable.HashMap.empty[Mod, Option[Version]]
  def versionOf(mod: Mod): Option[Version] = localVersions.getOrElseUpdate(mod, {
    val lv = if (mod.toString equalsIgnoreCase "if")  // "perl -e 'print $if::VERSION'" does not work
               Some(Version("0.0606"))
             else
               Try(Process( List("bash", "-c", s"$derivation/bin/perl -M$mod -e 'print $$$mod::VERSION' 2>/dev/null")
                          , cwd = None
                          , "PERL5LIB" -> s"$derivation/lib/perl5/site_perl"
                          ).!!.trim).toOption map (Version(_))
    //for (version <- lv)
    //  println(s"PerlDerivation($name): $mod = $version")
    lv
  })
}

import nixparser.{Token,Expr}
import Expr.{Val,Select,Apply,String1,String2,Ident,Attrs,KV,AntiQuote,AttrEntry,AttrKey,Inherited,InheritedFrom,BinOp,Assert,OpHasAttr,LetIn,URL,NixList}

// a typical code block in `perl-packages.nix`
class BuildPerlPackageBlock(val nixpkgsName:           String,
                            val source:                String,
                            val ast:                   Apply,
                            ) {
  case class WithToken[T](t: Token, x: T)
  private val Apply(vBuilder @ Val("buildPerlPackage"|"buildPerlModule"), inner:Attrs) = ast
  val wPname:                 WithToken[Name]                 = inner        .parts.collectFirst{ case    KV(Seq(Ident("pname"                )), e@ String1(Seq(Left(pnameString  )))) => WithToken(e, Name   (pnameString  ))                                        } getOrElse (throw new RuntimeException(s"no pname= in `${source}`"))
  val wVersion:               WithToken[Version]              = inner        .parts.collectFirst{ case    KV(Seq(Ident("version"              )), e@ String1(Seq(Left(versionString)))) => WithToken(e, Version(versionString))                                        } getOrElse (throw new RuntimeException(s"no version= in `${source}`"))
  val fetchurlAttrs:          Attrs                           = inner        .parts.collectFirst{ case    KV(Seq(Ident("src"                  )), Apply(                         Val("fetchurl"),        attrs:Attrs)) => attrs                                        } getOrElse (throw new RuntimeException(s"no src= in `${source}` `$inner`"))
  val wUrl:                   WithToken[String]               = fetchurlAttrs.parts.collectFirst{ case    KV(Seq(Ident("url"                  )), e@ String1(fragments))                => WithToken(e, fragments.map{
                                                                                                                                                                                                          case Left(s)                          => s
                                                                                                                                                                                                          case Right(AntiQuote(Val("pname"  ))) => pname.toString
                                                                                                                                                                                                          case Right(AntiQuote(Val("version"))) => version.toString
                                                                                                                                                                                                        }.mkString)
                                                                                                  case    KV(Seq(Ident("url"                  )), e@ URL    (urlString))                => WithToken(e, urlString)                                                     } getOrElse (throw new RuntimeException(s"no url= in `${fetchurlAttrs}`"))
  val wSha256:                WithToken[String]               = fetchurlAttrs.parts.collectFirst{ case    KV(Seq(Ident("sha256"               )), e@ String1(Seq(Left(sha))))           => WithToken(e, sha)                                                           } getOrElse (throw new RuntimeException(s"no sha256= in `${fetchurlAttrs}`"))
  val wPropagatedBuildInputs: Option[WithToken[List[String]]] = inner        .parts.collectFirst{ case e@ KV(Seq(Ident("propagatedBuildInputs")), NixList(parts))                       => //println(s"wPropagatedBuildInputs=`${e}`")
                                                                                                                                                                                           Some(WithToken(e, parts.toList.map{case x:Apply => "("+source.substring(x.start, x.end)+")"
                                                                                                                                                                                                                              case x       =>     source.substring(x.start, x.end) }))
                                                                                                  case e@ KV(Seq(Ident("propagatedBuildInputs")), _             )                       => ???
                                                                                                                                                                                                                                                                       } getOrElse None
  val wBuildInputs:           Option[WithToken[List[String]]] = inner        .parts.collectFirst{ case e@ KV(Seq(Ident("buildInputs"          )), NixList(parts))                       => //println(s"wBuildInputs=`${parts.toList.map(x => source.substring(x.start, x.end))}`")
                                                                                                                                                                                           Some(WithToken(e, parts.toList.map{case x:Apply => "("+source.substring(x.start, x.end)+")"
                                                                                                                                                                                                                              case x       =>     source.substring(x.start, x.end) }))
                                                                                                  case e@ KV(Seq(Ident("buildInputs"          )), _             )                       => ???
                                                                                                                                                                                                                                                                       } getOrElse None
  val wDoCheck:               Option[WithToken[      String]] = inner        .parts.collectFirst{ case e@ KV(Seq(Ident("doCheck"              )), _             )                       => //println(s"wDoCheck=`${source.substring(e.start, e.end)}`")
                                                                                                                                                                                                   Some(WithToken(e,                              source.substring(e.start, e.end)  )) } getOrElse None
  //println(s"nixpkgsName=$nixpkgsName wBuildInputs=$wBuildInputs inner=$inner")

  def pname   = wPname.x
  def version = wVersion.x
  def url     = wUrl.x

  def copy( builder:               String
          , pnameString:           String
          , versionString:         String
          , url:                   String
          , sha256:                SHA256
          , doCheckOverride:       Option[(Boolean, /*comment*/String)]
          , buildInputs:           List[String]
          , propagatedBuildInputs: List[String]
          , licenses:              Set[License]
          ): BuildPerlPackageBlock = {
    case class Patch(start: Int, end: Int, value: String)
    val patches = new collection.mutable.ArrayBuffer[Patch]

    patches += Patch(vBuilder.start,   vBuilder.end,   builder)
    patches += Patch(wPname.t.start,   wPname.t.end,   s"\042${pnameString}\042")
    patches += Patch(wVersion.t.start, wVersion.t.end, s"\042${versionString}\042")
    patches += Patch(wUrl.t.start,     wUrl.t.end,     s"\042${url}\042")
    patches += Patch(wSha256.t.start,  wSha256.t.end,  (wSha256.t.end-wSha256.t.start) match {
                                                         case 66 => s"\042${sha256.base16}\042"
                                                         case 54 => s"\042${sha256.base32}\042"
                                                       })

    (wDoCheck, doCheckOverride) match {
      case (_,                                 None                  ) => // keep unchanged
      case (None,                              Some((false, comment))) => // add `doCheck = false`
                                                                          patches += Patch(inner.end-1, inner.end-1, s"  doCheck = false; /* $comment */\n  ")
      case (Some(WithToken(t, _)),             Some((false, comment))) => // change existing `doCheck` to `false`
                                                                          patches += Patch(t.start, t.end, s"doCheck = false;")
      case (None | Some(WithToken(_, "true")), Some((true, _       ))) => // keep unchanged
      case (Some(WithToken(t, "false")),       Some((true, _       ))) => // change existing `doCheck` to `true` (the default)
                                                                          patches += Patch(t.start, t.end, "")
    }

    (wPropagatedBuildInputs, propagatedBuildInputs.nonEmpty) match {
      case (None                 , false) =>
      case (None                 , true ) => patches += Patch(inner.end-1, inner.end-1, s"  propagatedBuildInputs = [ ${propagatedBuildInputs mkString " "} ];\n  ")
      case (Some(WithToken(t, _)), false) => patches += Patch(t.start, t.end, "")
      case (Some(WithToken(t, _)), true ) => patches += Patch(t.start, t.end, s"propagatedBuildInputs = [ ${propagatedBuildInputs mkString " "} ];")
    }

    (wBuildInputs, buildInputs.nonEmpty) match {
      case (None                 , false) =>
      case (None                 , true ) => patches += Patch(inner.end-1, inner.end-1, s"  buildInputs = [ ${buildInputs mkString " "} ];\n  ")
      case (Some(WithToken(t, _)), false) => patches += Patch(t.start, t.end, "")
      case (Some(WithToken(t, _)), true ) => patches += Patch(t.start, t.end, s"buildInputs = [ ${buildInputs mkString " "} ];")
    }

//  (this.licenses, licenses.nonEmpty) match {
//    case (None   , false) =>
//    case (None   , true ) => // ???: insert licenses which were absent
//    case (Some(_), false) => // ???: remove licenses which were present
//    case (Some(_), true ) => // ???: replace existing license?
//                             //s = """(?s) license\s*=\s*(with[^;]+;\s*)[^;]+;""".r.replaceAllIn(s, s" license = with stdenv.lib.licenses; [ ${licenses mkString " "} ];")
//  }

    var s = source
    for (p <- patches.sortBy(-_.start)) {
      s = s.substring(0, p.start) + p.value + s.substring(p.end)
    }
    s = s.replaceAll(" +\n+", "\n")//.replaceAll("\n+", "\n")
    //println(s"s=[$s]")
    BuildPerlPackageBlock.fromString(s)
  }
}

object BuildPerlPackageBlock {
  def fromString(source: String): BuildPerlPackageBlock = Expr.attrentryFromString(source) match {
    case fastparse.Parsed.Success(KV(Seq(Ident(nixpkgsName)),         ast @ Apply(Val("buildPerlPackage"|"buildPerlModule"),_) ), _) => new BuildPerlPackageBlock(nixpkgsName, source, ast)
    case fastparse.Parsed.Success(KV(Seq(Ident(nixpkgsName)), LetIn(_,ast @ Apply(Val("buildPerlPackage"|"buildPerlModule"),_))), _) => new BuildPerlPackageBlock(nixpkgsName, source, ast)
    case e                                                                                                                           => throw new RuntimeException(s"BuildPerlPackageBlock.fromString($source) = $e")
  }
}



class PullRequester(repopath: File, supportedPerls: List[PerlDerivation]) {
  var `perl-packages.nix` = FileUtils.readFileToString(new File(repopath, "/pkgs/top-level/perl-packages.nix"), "UTF-8")
  var buildPerlPackageBlocks = collection.immutable.TreeMap.empty[String, BuildPerlPackageBlock]

  val fastparse.Parsed.Success(x, _) = Expr.fromString(`perl-packages.nix`)
  Expr.traverse(x::Nil) {
    case (outer @ KV(Seq(Ident(nixpkgsName)),         Apply(Val("buildPerlPackage"|"buildPerlModule"),inner:Attrs)) )::_ => Try(BuildPerlPackageBlock.fromString(`perl-packages.nix`.substring(outer.start, outer.end))) match {
                                                                                                                              case Success(b) => buildPerlPackageBlocks += nixpkgsName -> (b)
                                                                                                                              case Failure(e) => println(RED+e+RESET)
                                                                                                                            }
    case (outer @ KV(Seq(Ident(nixpkgsName)), LetIn(_,Apply(Val("buildPerlPackage"|"buildPerlModule"),inner:Attrs))))::_ => Try(BuildPerlPackageBlock.fromString(`perl-packages.nix`.substring(outer.start, outer.end))) match {
                                                                                                                              case Success(b) => buildPerlPackageBlocks += nixpkgsName -> (b)
                                                                                                                              case Failure(e) => println(RED+e+RESET)
                                                                                                                            }
    case _                                                                                                               =>
  }

  def nixifiedName(cp: CpanPackage) = buildPerlPackageBlocks.filter(_._2.pname == cp.pname).toList match {
    case Nil                                              => cp.suggestedNixpkgsName
    case (nixpkgsName, bppb)::Nil                         => nixpkgsName
    case blocks if cp.pname==Name("Archive-Zip")          => "ArchiveZip"
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
    else {
      val builtinsVersions: List[Option[Version]] = supportedPerls.map(_.versionOf(mod))
      if (builtinsVersions.nonEmpty && builtinsVersions.forall{ case Some(builtinsVersion) => version <= builtinsVersion
                                                                case None                  => false }) {
      //println(s"module $mod version $version is built-in in all perls ${supportedPerls.map(_.perlVersion) mkString " "}")
        None
      } else if (CpanErrata.modsToIgnore.get(mod).exists(_(version))) {
      //println(s"module $mod version $version ignored")
        None
      } else {
        Cpan.byMod(mod).toList match {
          case Nil                                                                     => throw new RuntimeException(s"mod `$mod' not found, maybe ${Cpan.byMod.keys filter (_.toString.toUpperCase.replaceAll("[:-]","") == mod.toString.toUpperCase.replaceAll("[:-]","")) mkString " "}");
          case cp::Nil if cp.pname.toString equalsIgnoreCase "perl"                    => None
          case cp::Nil if CpanErrata.namesToIgnore.get(cp.pname).exists(_(cp.version)) => //println(s"package $cp ignored")
                                                                                          None
          case cp::Nil                                                                 => CpanErrata.pinnedPackages.find(_.pname == cp.pname) match {
                                                                                            case Some(pinnedcp) => Some(pinnedcp)
                                                                                            case None           => Some(cp)
                                                                                          }
          case cpps                                                                    => throw new RuntimeException(s"mod `$mod' provided by many $cpps");
        }
      }
    }
  private def filterDeps(cp: CpanPackage, deps: Iterable[CpanPackage]) = for (d <- deps if d != cp;
                                                                                        if !(d.pname.toString.equalsIgnoreCase("Module-Build") && cp.isModule);
                                                                                        if !(CpanErrata.dependenciesToBreak(cp.pname) contains d.pname))
                                                                         yield d

  private def buildDeps      (cp: CpanPackage): Set[CpanPackage] = filterDeps(cp, cp.meta.buildMODs   ++ CpanErrata.extraBuildDependencies  (cp.pname) flatMap { case (m,v) => modToPackage(m,v) }).toSet
  private def runtimeDeps    (cp: CpanPackage): Set[CpanPackage] = filterDeps(cp, cp.meta.runtimeMODs ++ CpanErrata.extraRuntimeDependencies(cp.pname) flatMap { case (m,v) => modToPackage(m,v) }).toSet
  private def deepRuntimeDeps(cp: CpanPackage): Set[CpanPackage] = runtimeDeps(cp) flatMap (d => deepRuntimeDeps(d) + d)

  private val _allDeps = collection.mutable.HashMap.empty[CpanPackage, Set[CpanPackage]]
  def allDeps(cp: CpanPackage, seen: List[CpanPackage]=Nil): Set[CpanPackage] = _allDeps.getOrElseUpdate(cp, {
    if (seen contains cp) {
      println(s"circular dependency ${(cp::seen.takeWhile(_ != cp):::cp::Nil).reverse map (_.pname) mkString " -> "}")
      Set.empty
    } else {
      runtimeDeps(cp)++buildDeps(cp) flatMap (d => allDeps(d, cp :: seen) + d)
    }
  })


  private def updatedTo(b: BuildPerlPackageBlock, cp: CpanPackage): BuildPerlPackageBlock = {
//  println(s"`${b.source}`.updatedTo($cp)")
    b.copy( builder               = if (cp.isModule) "buildPerlModule" else "buildPerlPackage"
        , pnameString           = cp.pname.toString
        , versionString         = cp.version.toString stripPrefix "v"
        , url                   = s"mirror://cpan/authors/id/${cp.path}"
        , sha256                = cp.sha256
        , doCheckOverride       = CpanErrata.doCheckOverride get cp.pname
        , propagatedBuildInputs = b.wPropagatedBuildInputs.toList.flatMap(_.x).filter(name => name.contains("pkgs.") || name=="shortenPerlShebang") ++ (runtimeDeps(cp) -- runtimeDeps(cp).flatMap(deepRuntimeDeps _)).map(escapedNixifiedName).toArray.sorted
        , buildInputs           =           b.wBuildInputs.toList.flatMap(_.x).filter(name => name.contains("pkgs.") || name=="shortenPerlShebang") ++ (buildDeps(cp)   -- deepRuntimeDeps(cp)                       ).map(escapedNixifiedName).toArray.sorted
        , licenses              = cp.meta.licenses
        )
  }

  def fromCpanPackage(cp: CpanPackage): BuildPerlPackageBlock = {
    val sb = new java.lang.StringBuilder
    sb append s"""  ${cp.suggestedNixpkgsName} = ${if (cp.isModule) "buildPerlModule" else "buildPerlPackage"} {\n"""
    sb append s"""    pname = "${cp.pname}";\n"""
    sb append s"""    version = "${cp.version}";\n"""
    sb append s"""    src = fetchurl {\n"""
    sb append s"""      url = "mirror://cpan/authors/id/${cp.path}";\n"""
    sb append s"""      sha256 = "${cp.sha256.base32}";\n"""
    sb append s"""    };\n"""
    (runtimeDeps(cp) -- runtimeDeps(cp).flatMap(deepRuntimeDeps _)).toArray match {
      case Array() =>
      case a       => sb append s"""    propagatedBuildInputs = [ ${a.map(escapedNixifiedName).sorted mkString " "} ];\n"""
    }
    (buildDeps(cp)   -- deepRuntimeDeps(cp)                       ).toArray match {
      case Array() =>
      case a       => sb append s"""    buildInputs = [ ${a.map(escapedNixifiedName).sorted mkString " "} ];\n"""
    }
    CpanErrata.doCheckOverride.get(cp.pname) match {
      case Some((false, comment)) => sb append s"""      doCheck = false; /* $comment */\n"""
      case Some((true, _)) | None =>
    }
    sb append s"""    meta = {\n"""
    cp.meta.description foreach { text =>
      sb append s"""      description = "${text}";\n"""
    }
    if (cp.meta.licenses.nonEmpty) {
      sb append s"""      license = with stdenv.lib.licenses; [ ${cp.meta.licenses mkString " "} ];\n"""
    }
    cp.meta.homepage match {
      case Some(hp) if !hp.matches("""https?://metacpan\.org/.+""") =>
        sb append s"""      homepage = "${cp.meta.homepage.get}";\n"""
      case _                                                        =>
    }
    sb append s"""    };\n"""
    sb append s"""  };\n"""
    BuildPerlPackageBlock.fromString(sb.toString)
  }


  def prepareCommit(onp: Option[NixPackage], cp: CpanPackage, upgradeDeps: Boolean): Option[String] = {
    allDeps(cp) // to fail earlier if circular deps found

/**
    // debug print
    println(s"  prepareCommit($onp, $cp)")
    println(allDeps(cp) mkString "\n")
    println(s"  meta.buildMODs:")
    println(cp.meta.buildMODs mkString "\n")
    println(s"  buildDeps:")
    println(buildDeps(cp) mkString "\n")
    println(s"  meta.runtimeMODs:")
    println(cp.meta.runtimeMODs mkString "\n")
    println(s"  runtimeDeps:")
    println(runtimeDeps(cp) mkString "\n")

    onp match {
      case Some(np) =>
        buildPerlPackageBlocks find (_._2.url == np.url) match {
          case Some((_, block)) =>
            println(s"======================== ${np.pname}: ${np.version} -> ${cp.version}")
            println(block   .source split '\n' map ("< "+_) mkString "\n")
        }
    }
**/

    def isBuiltInPerl: Boolean = {
      val builtinsMods: Map[Mod, (Version, List[Option[Version]])] = Cpan.providedMods(cp) map { case (mod, cpanVersion) => mod -> (cpanVersion, supportedPerls.map(_.versionOf(mod))) }
      val rc = builtinsMods.nonEmpty &&
               builtinsMods.forall{ case (mod, (cpanVersion, builtinsVersions))  => builtinsVersions.forall{ case Some(builtinsVersion) => cpanVersion <= builtinsVersion
                                                                                                             case None                  => false }
                                    case _                                       => false
                                  }
//      if (rc) {
//        println(s"| $np -> $cp")
//        for (x <- tosmMods)
//          println(s"|   $x")
//      }
      rc
    }

    var newBlock: BuildPerlPackageBlock = null
    onp match {
      case Some(np) =>
        buildPerlPackageBlocks find (_._2.url == np.url) match {
          case Some((_, block)) if isBuiltInPerl =>
            // do mutate `perl-packages.nix`
            `perl-packages.nix` = `perl-packages.nix`.replace(block.source.trim, s"""${block.nixpkgsName} = null; # part of Perl ${supportedPerls.map(_.perlVersion) mkString ", "}""")

            val pw = new java.io.PrintWriter(new File(repopath, "/pkgs/top-level/perl-packages.nix"))
            pw write `perl-packages.nix`
            pw.close()

            return Some(s"perlPackages.${block.nixpkgsName}: removed built-in")
          case Some((_, block)) =>
            newBlock = updatedTo(block, cp)

            // do mutate `perl-packages.nix`
            buildPerlPackageBlocks = buildPerlPackageBlocks - block.nixpkgsName + (newBlock.nixpkgsName -> newBlock)
            `perl-packages.nix` = `perl-packages.nix`.replace(block.source.trim, newBlock.source.trim)
          case None =>
            System.err.println(s"$np->$cp not found in perl-packages.nix")
            return None
        }
      case None =>
        newBlock = fromCpanPackage(cp)

        // do mutate `perl-packages.nix`
        buildPerlPackageBlocks = buildPerlPackageBlocks + (newBlock.nixpkgsName -> newBlock)
        val after = (buildPerlPackageBlocks.until(newBlock.nixpkgsName).lastOption getOrElse buildPerlPackageBlocks.last)._2
        `perl-packages.nix` = `perl-packages.nix`.replace(after.source.trim, after.source.trim+"\n\n  "+newBlock.source.trim)
    }

    var message = List.empty[String]
    onp match {
      case Some(np) if np.version != cp.version => message ::= s"perlPackages.${newBlock.nixpkgsName}: ${np.version} -> ${cp.version}"
      case Some(np)                             => message ::= s"perlPackages.${newBlock.nixpkgsName}: cleanup"
      case None                                 => message ::= s"perlPackages.${newBlock.nixpkgsName}: init at ${cp.version}"
    }

    val depBlocks: Set[Either[(BuildPerlPackageBlock, CpanPackage), BuildPerlPackageBlock]] =
      allDeps(cp) flatMap { dep =>
        buildPerlPackageBlocks.filter(_._2.pname == dep.pname).toList match {
          case Nil        => Right(fromCpanPackage(dep)) :: Nil // Try(new BuildPerlPackageBlock(dep)).toOption.map(Right(_))
          case depblocks  => //if (depblocks.length>1) println("depblocks:"::depblocks mkString "\n")
                             depblocks map {case (_, bppb) => Left(bppb->dep)}
        }
      }

/*
    // debug print
    println(newBlock.source split '\n' map ("> "+_) mkString "\n")
    depBlocks foreach {
      case Left((bppb, dep)) => // upgrade/cleanup existing dep
        val newBppb = updatedTo(bppb, dep)
        if (bppb.source.trim != newBppb.source.trim) {
          println(bppb   .source split '\n' map ("< "+_) mkString "\n")
          println(newBppb.source split '\n' map ("> "+_) mkString "\n")
        }
      case Right(newBppb)       => // insert new dep
        println(newBppb.source split '\n' map ("+ "+_) mkString "\n")
    }
*/

    depBlocks foreach {
      case Left((bppb, dep)) => // upgrade/cleanup existing dep
        if (upgradeDeps) {
          val newBppb = updatedTo(bppb, dep)
          if (bppb.source.trim != newBppb.source.trim) {
            if (bppb.version != newBppb.version)
              message ::= s"perlPackages.${bppb.nixpkgsName}: ${bppb.version} -> ${newBppb.version}"
            else
              message ::= s"perlPackages.${bppb.nixpkgsName}: cleanup"

            require(dep.pname == newBppb.pname, s"${dep.pname} => ${newBppb.pname}")
            buildPerlPackageBlocks = buildPerlPackageBlocks - bppb.nixpkgsName + (newBppb.nixpkgsName -> newBppb)
            `perl-packages.nix` = `perl-packages.nix`.replace(bppb.source.trim, newBppb.source.trim)
          }
        }

      case Right(bppb) => // insert new dep
        buildPerlPackageBlocks += bppb.nixpkgsName -> bppb
        val after = (buildPerlPackageBlocks.until(bppb.nixpkgsName).lastOption getOrElse buildPerlPackageBlocks.last)._2
        if (CpanErrata.inExternalNixFiles contains bppb.pname) {
          //`perl-packages.nix` = `perl-packages.nix`.replace(after.source.trim, after.source.trim+"\n/*\n  "+bppb.source.trim+"\n*/")
          //message ::= s"perlPackages.${bppb.nixpkgsName}: init at ${bppb.version}"
        } else {
          `perl-packages.nix` = `perl-packages.nix`.replace(after.source.trim, after.source.trim+"\n\n  "+bppb.source.trim)
          message ::= s"perlPackages.${bppb.nixpkgsName}: init at ${bppb.version}"
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



object Worker {
  sealed trait Location
  case object Local                                                    extends Location
  case class Remote(user: String, host: String, sshopts: List[String]) extends Location {
    require(user matches """[a-z][a-z0-9]*""")
    require(host matches """[a-z0-9-]+(\.[a-z0-9-]+)*""")
  }
}

case class Worker(system: String, concurrency: Int, location: Worker.Location) {
  require(Set("x86_64-linux", "i686-linux", "armv7l-linux", "aarch64-linux", "x86_64-darwin") contains system)
}


// 1e62e86b65fde74c23afa4304ba1fcab950d99cf
object Cpan2Nix {
//val builder_X86_64  = Worker("x86_64-linux",   12, Worker.Local)
//val builder_I686    = Worker("i686-linux",     12, Worker.Local)
  val builder_X86_64  = Worker("x86_64-linux",   12, Worker.Remote("root",     "x13.lan",                 Nil))
  val builder_I686    = Worker("i686-linux",     12, Worker.Remote("root",     "x13.lan",                 Nil))
//val builder_DARWIN  = Worker("x86_64-darwin",   4, Worker.Remote("user",     "172.16.224.2",            Nil))
  val builder_AARCH32 = Worker("armv7l-linux",  100, Worker.Remote("volth",    "aarch64.nixos.community", Nil))
  val builder_AARCH64 = Worker("aarch64-linux", 100, Worker.Remote("volth",    "aarch64.nixos.community", Nil))

  // todo: command-line switches
  val doCheckout  = true
  val doInsert    = /*"Net-Amazon-EC2" ::*/ Nil
  val doUpgrade   = true
  val doTestBuild: List[Worker] = // builder_AARCH64 ::
                                  // builder_AARCH32 ::
                                  // builder_I686    ::
                                     builder_X86_64  ::
                                  Nil



  def main(args: Array[String]) {
    args match {
      case Array()                   => main(Array("--repopath", "./nixpkgs-repo"))
      case Array("--repopath", path) =>
        val repopath: File = new File(path)

        if (doCheckout) {
          if (!repopath.exists) {
            require(Process("git" :: "clone" :: "https://github.com/nixos/nixpkgs" :: repopath.getAbsolutePath :: Nil).! == 0)
          } else {
            require(Process("git" :: "fetch" :: "origin" :: "staging" :: Nil, cwd = repopath).! == 0)
//          require(Process("git" :: "fetch" :: "origin" :: "staging-20.09" :: Nil, cwd = repopath).! == 0)
          }

          val branchName = { val now = new java.util.Date; f"cpan2nix-${1900+now.getYear}%04d-${1+now.getMonth}%02d-${now.getDate}%02d" }
          require(Process("git" :: "checkout" :: "-f"        :: "remotes/origin/staging"                       :: Nil, cwd = repopath).! == 0)
//        require(Process("git" :: "checkout" :: "-f"        :: "remotes/origin/staging-20.09"                 :: Nil, cwd = repopath).! == 0)


//        require(Process("git" :: "cherry-pick"             :: "df55a4aa20c813625bd9bbf46ffb7d77dd089bba"     :: Nil, cwd = repopath).! == 0)
//        require(Process("git" :: "checkout" :: "-f"        :: "remotes/origin/master"                        :: Nil, cwd = repopath).! == 0)
                 (Process("git" :: "cherry-pick"             :: "d34fb0a99d493d3c0cb19e4825869cf437bf3916"     :: Nil, cwd = repopath).! == 0)
                 (Process("git" :: "cherry-pick"             :: "4a6de80c212eed02dc320f077032704c92afc216"     :: Nil, cwd = repopath).! == 0)

          require(Process("git" :: "branch"   :: "-f"        :: branchName :: "HEAD"                           :: Nil, cwd = repopath).! == 0)
          require(Process("git" :: "checkout" ::                branchName                                     :: Nil, cwd = repopath).! == 0)
        }

        val nixPkgs = new NixPkgs(repopath.getAbsolutePath)

//      val nixPkgs = new NixPkgs("https://github.com/NixOS/nixpkgs/archive/staging.tar.gz")

        val canUpgradeMemo = collection.mutable.Map.empty[NixPackage, Option[CpanPackage]]
        def canUpgrade(np: NixPackage): Option[CpanPackage] = canUpgradeMemo.getOrElseUpdate(np, {
          CpanErrata.namesToIgnore.get(np.pname) match {
            case Some(pred) if pred(np.version) =>
              None
            case _ =>
              CpanErrata.pinnedPackages find (_.pname == np.pname) orElse {
                np.maybeauthor match {
                  case Some(author) =>
                    Cpan.byAuthorAndName(author->np.pname) match {
                      case cpps if cpps.isEmpty                         =>
                        Cpan.byName(np.pname) match {
                          case cpps if cpps.isEmpty                          => val mod = Mod(np.pname.toString.replace("-", "::")) // try to understand as module name
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
                        Cpan.byName(np.pname) match {
                          case cpps if cpps exists (np.version < _.version) => System.err.println(f"${np.url}%-90s other authors have newer versions ${cpps.filter(np.version < _.version).groupBy(_.author.toString).mapValues(_.map(_.version).toList.sorted) mkString ", "}")
                                                                               Some(cpps.maxBy(_.version))
                          case _                                            => Some(cpps.find(_.version == np.version).get)
                        }
                      case cpps if cpps forall (_.version < np.version) =>
                        Cpan.byName(np.pname) match {
                          case cpps if cpps exists (np.version < _.version) => Some(cpps.maxBy(_.version))
                          case _                                            => System.err.println(f"${np.url}%-90s version not found in CPAN; there are only ${cpps.map(_.version).toArray.sorted mkString ", "}")
                                                                               Some(cpps.maxBy(_.version))
                        }
                    }
                  case None => // author is not specified in nixpkgs
                    Cpan.byName(np.pname) match {
                      case cpps if cpps.isEmpty                          => throw new RuntimeException(s"${np.url} not found in CPAN")
                      case cpps if cpps exists (np.version <= _.version) => Some(cpps.maxBy(_.version))
                      case cpps if cpps forall (_.version < np.version)  => throw new RuntimeException(s"${np.url} not found in CPAN; there are only ${cpps.map(_.version).toArray.sorted mkString ", "}")
                    }
                }
              }
          }
        })

        val supportedPerls = new PerlDerivation(repopath, name="perl530",   perlVersion="5.30.3") ::
                             new PerlDerivation(repopath, name="perl532",   perlVersion="5.32.0") ::
                             new PerlDerivation(repopath, name="perldevel", perlVersion="5.33.1") :: Nil
        val pullRequester = new PullRequester(repopath, supportedPerls)

        // compare results of evaluation pkgs.perlPackages (nixPkgs.allPackages) and parsing of perl-packages.nix (pullRequester.buildPerlPackageBlocks)
        for (np <- nixPkgs.allPackages if !pullRequester.buildPerlPackageBlocks.exists(_._2.pname == np.pname)) {
          println(s"WARNING: evaluated but not parsed (probably not in perl-packages.nix): $np")
        }
        for ((_, bppb) <- pullRequester.buildPerlPackageBlocks if !nixPkgs.allPackages.exists(_.pname == bppb.pname)) {
          println(s"WARNING: parsed but not evaluated (probably not from CPAN): perlPackages.${bppb.nixpkgsName}")
        }

        if (doInsert.nonEmpty) {
          val toadd = doInsert flatMap (name => Cpan.byName(Name(name)))
          for (cp      <- toadd;
               message <- pullRequester.prepareCommit(None, cp, upgradeDeps = false)) {
            println("----")
            println(message)

            Process("git" :: "commit" :: "-m" :: s"[cpan2nix] $message" :: "pkgs/top-level/perl-packages.nix" :: Nil,
                    cwd = repopath).!
          }
        }
/**
//      for (np <- nixPkgs.allPackages) {
//        println(np)
//      }
        for ((name,cps) <- Cpan.byName.toArray.sortBy(_._1.toString)) {
          println(name)
          for (cp <- cps) {
            println(cp.metaFile)
            for ((a,b) <- cp.meta.runtimeMODs) {
              println(" ", a, b)
            }
            println()
          }
        }
        sys exit 1
**/
        if (doUpgrade) {
/**/
          val toupdate = nixPkgs.allPackages sortBy { case np if np.pname.toString equalsIgnoreCase "XML-SAX" => (0, 0)                  // XML-SAX first, it is an indirect dependency of many others via `pkgs.docbook'
                                                      case np if np.pname.toString equalsIgnoreCase "JSON"    => (1, 0)                  // JSON second, others depends on it via `pkgs.heimdal'
                                                      case np                                                 => canUpgrade(np) match {
                                                                                                                   case Some(cp) => (10, pullRequester.allDeps(cp).size)  // then smaller first
                                                                                                                   case None     => (20, 0)
                                                                                                                 }
                                                    }
/**/
//        val toupdate = nixPkgs.allPackages filter (np => np.pname == Name("SVN-Simple"))


          for (np      <- toupdate;
               cp      <- canUpgrade(np);
               message <- pullRequester.prepareCommit(Some(np), cp, upgradeDeps = true)) {
            println("----")
            println(message)

            Process("git" :: "commit" :: "-m" :: s"[cpan2nix] $message" :: "pkgs/top-level/perl-packages.nix" :: Nil,
                    cwd = repopath).!
          }

//        require(Process("git" :: "push" :: "-f" :: "git@github.com:/volth/nixpkgs"                     :: Nil, cwd = repopath).! == 0)
          require(Process("git" :: "cherry-pick"             :: "b235c10a7536c511a1519064770dc13dcc31a155"     :: Nil, cwd = repopath).! == 0)
          require(Process("git" :: "cherry-pick"             :: "80ad38e79ebd775e23d9a5571ffaf5815bbfe2ae"     :: Nil, cwd = repopath).! == 0)
        }


        for (worker <- doTestBuild) {
          // try to build
          val nixcode = s"""|let
                            |# pkgs    = import <nixpkgs> { config.checkMetaRecursively = true; config.allowAliases = false; };
                            |  # do the build als ob the perl version is bumped
                            |# pkgs530 = import <nixpkgs> { system = "${worker.system}"; config.checkMetaRecursively = true; config.allowUnfree = true; config.oraclejdk.accept_license = true; overlays = [ (self: super: { perl = self.pkgs530; perlPackages = self.perl530Packages; }) ]; };
                            |  pkgs532 = import <nixpkgs> { system = "${worker.system}"; config.checkMetaRecursively = true; config.allowUnfree = true; config.oraclejdk.accept_license = true;                                                                                              };
                            |  inherit (pkgs532) lib;
                            |in
                            |   lib.concatMap ({pkgs, dotperl}: [
                            |     pkgs.nix-serve
                            |   # pkgs.hydra
                            |     ((dotperl pkgs).withPackages(p: lib.filter
                            |                                  (x: (x != null) && (lib.isDerivation x) && x.meta.available)
                            |                                  [
                            |                                    ${ pullRequester.buildPerlPackageBlocks flatMap {
                                                                      case ( "RegexpCopy"                  // 2003
                                                                           | "libfile-stripnondeterminism" // need manual upgrade
                                                                           | "strip-nondeterminism"
                                                                           , _)       => Nil
                                                                      case (name, bp) => List("p." + bp.nixpkgsName)
                                                                    } mkString " "
                                                                 }
                            |                                  ]
                            |                            ))
                            |   ] ++ lib.optionals pkgs.stdenv.is64bit [
                            |""".stripMargin +
         (worker.system match {
            case "x86_64-linux" =>
                        s"""|
                            |     ((dotperl pkgs.pkgsCross.raspberryPi            ).withPackages(p: [p.LWP p.XMLParser]))
                            |     ((dotperl pkgs.pkgsCross.armv7l-hf-multiplatform).withPackages(p: [p.LWP p.XMLParser]))
                            |     ((dotperl pkgs.pkgsCross.aarch64-multiplatform  ).withPackages(p: [p.LWP p.XMLParser]))
                            |    #((dotperl pkgs.pkgsCross.armv7l-hf-multiplatform).pkgs.ModuleBuild)
                            |     ((dotperl pkgs.pkgsMusl                         ).withPackages(p: [p.LWP p.XMLParser]))
                            |""".stripMargin
            case "aarch64-linux" =>
                        s"""|
                            |     ((dotperl pkgs.pkgsCross.raspberryPi            ).withPackages(p: [p.LWP p.XMLParser]))
                            |     ((dotperl pkgs.pkgsCross.armv7l-hf-multiplatform).withPackages(p: [p.LWP p.XMLParser]))
                            |""".stripMargin
            case _              => ""
          }) +
                        s"""|   ]
                            |   )
                            |   [
                            |   # {pkgs = pkgs532; dotperl = p: p.perl530;  }
                            |     {pkgs = pkgs532; dotperl = p: p.perl532;  }
                            |   # {pkgs = pkgs532; dotperl = p: p.perldevel;}
                            |   ]
                            |""".stripMargin

          println(nixcode)

          val instantiateDrvs = Task[List[String]](
                                  Process( "nix-instantiate"
                                           :: "--show-trace"
                                           :: "-E" :: nixcode :: Nil,
                                           cwd = repopath,
                                           "NIXPKGS_CONFIG" -> "",
                                           "NIX_PATH"       -> s"nixpkgs=${repopath.getAbsolutePath}"
                                         ).!!.split('\n').toList
                                )

          val t = worker match {
                    case Worker(system, concurrency, Worker.Remote(user, host, sshopts)) =>
                      for (drvs <-  instantiateDrvs;

                           _ <- Task(require(Process("nix-copy-closure" :: "-v" :: "--to" :: s"$user@$host" :: drvs,
                                                     cwd = repopath,
                                                      "NIX_SSHOPTS" -> sshopts.mkString(" ")).! == 0));

                           // split `drvs` to avoid too long command line (workaround for https://github.com/NixOS/nix/issues/2256)
                           _ <- Task.wander(drvs.grouped(1000).toList) { slice =>
                                  Task {
                                    val cmd = ("ssh" :: "-tt" // allocate remote tty so local Ctrl-C would kill the remote build
                                                     :: sshopts ::: s"$user@$host" :: "--"
                                                     :: "nix-store" :: "--realise" /*:: "--ignore-unknown"*/
                                                     :: "--sandbox"
                                                     :: "--option"  :: "binary-caches" :: "http://cache.nixos.org/"
                                                     :: "--extra-platforms" :: system // forcing build for the system (needed for "armv7l-linux" and "i686-linux")
                                                     :: s"-j${concurrency}"
                                                     :: (if (concurrency == 1) List() else List("--keep-going"))
                                                    ::: slice)
                                    require(Process(cmd).! == 0)
                                  }.materialize
                                }
                          /* copy the results back
                           _ <- Task( require(Process("nix-copy-closure" :: "-v" :: "--include-outputs" :: "--from" :: s"$user@$host" :: drvs,
                                                      cwd = repopath,
                                                      "NIX_SSHOPTS" -> NIX_SSHOPTS.mkString(" ")).! == 0)
                          */
                          ) yield ()
                    case Worker(system, concurrency, Worker.Local) =>
                      for (drvs <- instantiateDrvs;
                           // split `drvs` to avoid too long command line (workaround for https://github.com/NixOS/nix/issues/2256)
                           _    <- Task.traverse(drvs.grouped(1000).toList) { slice =>
                                     Task {
                                       require(Process( "nix-store" :: "--realise" /*:: "--ignore-unknown"*/
                                                     :: "--sandbox"
                                                     :: "--option"  :: "binary-caches" :: /* http://$worker:44444/ */ s"http://cache.nixos.org/"
                                                     :: "--keep-failed"
                                                     :: s"-j${concurrency}"
                                                     :: (if (concurrency == 1) List() else List("--keep-going"))
                                                    ::: slice).! == 0)
                                     }
                                   }
                          ) yield ()
          }
          val io = Scheduler.io("my-io") // do not limit parallelism with number of local cpus
          Await.result(t.runAsync(io), 10.hours)
        }

      case _ =>
        println(s"unexpected args: ${args.toList}")
    }
  }
}
