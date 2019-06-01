# let plan = import ./plan.nix; in
# { pkg-def = plan;
#   overlay =
#     { tttool = ./tttool.nix;
#       HPDF = ./HPDF2.nix;
#     };
# }

{ pkgs ? import <nixpkgs> {}
, haskell
, iohk-module
, iohk-extras
, ...
}:
let

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  plan = import ./plan.nix;
  compiler = (plan haskell).compiler.nix-name;
  pkgSet = haskell.mkPkgSet {
    pkg-def = plan;
    pkg-def-extras = [
      iohk-extras.${compiler}
      { bSpokeLight = ./bSpokeLight.nix;
      }
      # for windows builds
      (hackage: { Win32 = hackage.Win32."2.6.2.0".revisions.default; })
      (hackage: { mintty = hackage.mintty."0.1.2".revisions.default; })
      # for iserve-remote
      (hackage: { network = hackage.network."2.8.0.0".revisions.default; })
      ];
    modules = [
      # haskell.ghcHackagePatches.${compiler}
      iohk-module
      {
        packages.bSpokeLight.configureFlags =
          # Horrible hack
          [ "--ghc-option=-DFIRMWARE=\\\"${pkgs.bSpokeLight-firmware}\\\"" ] ++
          # Configure static building of bSpokeLight
          pkgs.lib.optionals (pkgs.hostPlatform.isMusl) [
           "--ghc-option=-static"
           "--ghc-option=-optl=-static"
           "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
           "--extra-lib-dirs=${pkgs.zlib.static}/lib"
         ];
      }
    ];
  };
in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
