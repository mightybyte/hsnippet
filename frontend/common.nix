{ haskellPackages, fetchgit, extraBuildInputs ? [] }:
haskellPackages.mkDerivation {
  pname = "hsnippet-frontend";
  version = "0.1";
  src = ./.;
  preConfigure = ''
    ln -sfT "${../shared/src}" ./src-shared
  '';
  isExecutable = true;
  isLibrary = false;
  buildDepends = with haskellPackages; [
    aeson
    cereal
    comonad
    data-default
    dependent-sum-template
    errors
    diagrams-lib
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
