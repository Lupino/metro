{ static ? false, compiler ? "default" }:

let
  config = import ./nix/config.nix {static=static;};

  pkgs = config.pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

in {
  metro = haskellPackages.callPackage ./nix/metro.nix {
    static = static;
  };
}
