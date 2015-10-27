#!/bin/sh

if ! command -v nix-shell >/dev/null ; then
  echo Setting up environment
  . ~/.nix-profile/etc/profile.d/nix.sh
fi

nix-shell -A env --pure -j 8 -I ../deps --command "cabal configure --ghcjs && cabal build | grep -v ^Linking && cp dist/build/hsnippet-frontend/hsnippet-frontend.jsexe/all.js ./hsnippet.js"
