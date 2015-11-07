{ haskellPackages, fetchgit, extraBuildInputs ? [] }:
haskellPackages.mkDerivation {
  pname = "hsnippet-builder";
  version = "0.1";
  src = ./.;
  isExecutable = true;
  isLibrary = false;
  buildDepends = with haskellPackages; [
    aeson
    cereal
    comonad
    data-default
    dependent-sum-template
    diagrams-lib
    errors
    ghc
    ghcjs-dom
    hsnippet-lib
    http-types
    lens
    monad-loops
    readable
    reflex
    reflex-dom
    reflex-dom-contrib
    safe
    safecopy
    scientific
    semigroups
    split
    time
    unordered-containers
    generic-deriving
  ] ++ extraBuildInputs;
  license = null;
  passthru = {
    inherit haskellPackages;
  };
}
