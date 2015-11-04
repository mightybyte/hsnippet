{ reflex-platform ? import ../deps/reflex-platform {} }:
let nixpkgs = reflex-platform.nixpkgs;
in import ./common.nix {
  haskellPackages = import ./packages.nix {
    pkgs = nixpkgs;
    haskellPackages = reflex-platform.ghcjs;
  };
  inherit (nixpkgs) fetchgit;
  extraBuildInputs = with reflex-platform.ghc; [ cabal-install ghc ];
}
