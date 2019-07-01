{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:

  let sourceByRegex = import ./source-by-regex.nix pkgs; in

  let src = sourceByRegex ../. [
      "bSpokeLight.hs"
      "bSpokeLight.cabal"
      "ChangeLog.md"
      "LICENSE"
      ]; in

  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bSpokeLight"; version = "0.1"; };
      license = "MIT";
      copyright = "2013-2019 Joachim Breitner";
      maintainer = "mail@joachim-breitner.de";
      author = "Joachim Breitner";
      homepage = "https://github.com/nomeata/bSpokeLight";
      url = "";
      synopsis = "Custom firmware for the YQ8003 bicycle spoke light";
      description = "This project provides an alternative firmware for LED spoke lights. Currently, only the model YQ8003 (128 LEDs) is supported.";
      buildType = "Simple";
      };
    components = {
      exes = {
        "bSpokeLight" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.template-haskell)
            (hsPkgs.regex-posix)
            ];
          };
        };
      };
    } // rec { inherit src; }
