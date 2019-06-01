let localLib = import ./lib.nix; in

let
  bSpokeLight-exe = pkgs:
    let
      # haskellLib = pkgs.fetchFromGitHub {
      #   owner  = "input-output-hk";
      #   repo   = "haskell.nix";
      #   rev    = "8ee6fcfba7bb220e8d6e19106ad2ae2c25ecdf43";
      #   sha256 = "1ndpxyairppcr3mnb4zi7gsvdqmncp09fgxdk8cbrh7ccb1r30kz";
      #   fetchSubmodules = false;
      #   name   = "haskell-lib-source";
      # };
      # haskell = import haskellLib { inherit pkgs; };
      haskell = localLib.nix-tools.haskell { inherit pkgs; };
      iohk-module = localLib.nix-tools.iohk-module;
      iohk-extras = localLib.nix-tools.iohk-extras;
      nix-tools = import ./pkgs.nix { inherit pkgs haskell iohk-module iohk-extras; };
    in
    nix-tools.bSpokeLight.components.exes.bSpokeLight;
in

let
  overlay = self: super:
    {
      bSpokeLight-firmware = pkgs.stdenv.mkDerivation {
        name = "bSpokeLight-firmware";

        src = sourceByRegex ../firmware [
          "Makefile"
          "firmware.c"
        ];

        buildInputs = [ pkgs.sdcc ];

        installPhase = ''
          mkdir -p $out
          cp firmware.bin $out
          cp firmware.map $out
        '';
      };
    };
  getPkgs = opts: localLib.iohkNix.getPkgs (opts // { extraOverlays = [ overlay ];});

  pkgs         = getPkgs {};
  pkgs-static  = getPkgs { crossSystem = localLib.systems.examples.musl64; };
  pkgs-windows = getPkgs { crossSystem = localLib.systems.examples.mingwW64; };
  pkgs-osx     = getPkgs { system = "x86_64-darwin"; };

  sourceByRegex = import ./source-by-regex.nix pkgs;

in rec {
  firmware = pkgs.bSpokeLight-firmware;

  linux-exe = bSpokeLight-exe pkgs;
  windows-exe = bSpokeLight-exe pkgs-windows;
  static-exe = bSpokeLight-exe pkgs-static;

  static-files = sourceByRegex ../. [
    "README.md"
    "imgs/"
    "imgs/.*\.png"
    "imgs/.*\.gif"
  ];

  contrib = ../contrib;

  release = pkgs.stdenv.mkDerivation {
    name = "bSpokeLight-release";

    buildInputs = [ pkgs.perl ];

    builder = pkgs.writeScript "create-bSpokeLight-release.sh" ''
      source ${pkgs.stdenv}/setup

      mkdir -p $out/
      cp -vsr ${static-files}/* $out
      cp -vs ${static-exe}/bin/bSpokeLight $out/
    '';
      # cp -vs ${windows-exe}/bin/bSpokeLight.exe $out/
  };

  release-zip = pkgs.stdenv.mkDerivation {
    name = "bSpokeLight-release.zip";

    buildInputs = with pkgs; [ perl zip bash ];

    builder = pkgs.writeScript "zip-bSpokeLight-release.sh" ''
      source ${pkgs.stdenv}/setup

      #version=$(bash ${release}/bSpokeLight --help|perl -ne 'print $1 if /bSpokeLight-(.*) -- The swiss army knife/')
      version=0.0
      base="bSpokeLight-$version"
      mkdir -p $out/$base
      cd $out
      cp -r ${release}/* $base/
      chmod u+w -R $base
      zip -r $base.zip $base
      rm -rf $base
    '';
  };

}
