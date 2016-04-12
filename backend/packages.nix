{ mkDerivation, aeson, base, bytestring, configurator, containers
, directory, errors, filepath, fsnotify, groundhog
, groundhog-postgresql, groundhog-th, heist, lens, monad-logger
, mtl, postgresql-simple, process, random, readable, resource-pool
, rng-utils, SHA, snap, snap-core, snap-loader-static, snap-server
, snaplet-postgresql-simple, split, stdenv, string-conv, text, time
, transformers, websockets, websockets-snap
}:
mkDerivation {
  pname = "hsnippet-backend";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring configurator containers directory errors
    filepath fsnotify groundhog groundhog-postgresql groundhog-th heist
    lens monad-logger mtl postgresql-simple process random readable
    resource-pool rng-utils SHA snap snap-core snap-loader-static
    snap-server snaplet-postgresql-simple split string-conv text time
    transformers websockets websockets-snap
  ];
  description = "Haskell code snippet app";
  license = stdenv.lib.licenses.bsd3;

  preConfigure = ''
    ln -sfT "${../shared/src}" ./src-shared
  '';

}
