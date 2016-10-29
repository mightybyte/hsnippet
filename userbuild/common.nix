{ haskellPackages, fetchgit, extraBuildInputs ? [] }:
haskellPackages.mkDerivation {
  pname = "hsnippet-builder";
  version = "0.1";
  src = ./.;
  isExecutable = false;
  isLibrary = false;
  buildDepends = with haskellPackages; [
    aeson
    cereal
    colour
    comonad
    data-default
    dependent-sum-template
    diagrams-lib
    diagrams-reflex
    errors
    ghc
    ghcjs-dom
    hsnippet-lib
    http-types
    lens
    linear
    monad-loops
    palette
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
