#!/bin/sh

if ! command -v nix-shell >/dev/null ; then
  echo Setting up environment
  . ~/.nix-profile/etc/profile.d/nix.sh
fi

../deps/reflex-platform/work-on ./overrides.nix ./. --run "cabal configure --ghcjs && cabal build"
cp dist/build/hsnippet-frontend/hsnippet-frontend.jsexe/rts.js .
cp dist/build/hsnippet-frontend/hsnippet-frontend.jsexe/lib.js .
cat dist/build/hsnippet-frontend/hsnippet-frontend.jsexe/out.js dist/build/hsnippet-frontend/hsnippet-frontend.jsexe/runmain.js > hsnippet.js
