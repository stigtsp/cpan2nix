#!/usr/bin/env nix-shell
#! nix-shell -i bash --packages scala jre coursier wget gnutar git

export CLASSPATH=$(coursier fetch --classpath io.circe:circe-parser_2.12:0.9.1 org.yaml:snakeyaml:1.20)
tail +15 Cpan2Nix.scala > CPANTesterTmp.scala
scalac -cp $CLASSPATH -d Cpan2Nix.jar CPANTesterTmp.scala
rm CPANTesterTmp.scala
exec java -cp $CLASSPATH:$(coursier fetch --classpath org.scala-lang:scala-library:2.12.6):Cpan2Nix.jar Cpan2Nix "$@"
