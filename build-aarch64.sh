#!/bin/sh

drvs=$(nix-instantiate --show-trace -K -k -E 'with (import /home/user/m/cpan2nix/nixpkgs-repo { system = "aarch64-linux"; }); [ perlPackages.GnuPG perl528Packages.GnuPG perldevelPackages.GnuPG ]')
nix-copy-closure -v --to volth@aarch64.nixos.community $drvs
echo $drvs
ssh -t -t volth@aarch64.nixos.community -- nix-store -r $drvs

