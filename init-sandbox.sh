#! /bin/sh

cabal sandbox init

cabal sandbox add-source deps/snaplet-postgresql-simple

cabal sandbox add-source deps/snap/deps/io-streams
cabal sandbox add-source deps/snap/deps/io-streams-haproxy
cabal sandbox add-source deps/snap/deps/snap-core
cabal sandbox add-source deps/snap/deps/snap-server
cabal sandbox add-source deps/snap/deps/xmlhtml
cabal sandbox add-source deps/snap/deps/heist
cabal sandbox add-source deps/snap
cabal sandbox add-source deps/snap-loader-dynamic
cabal sandbox add-source deps/snap-loader-static

# cabal sandbox add-source deps/servant-snap/deps/servant/servant
# cabal sandbox add-source deps/servant-snap/deps/servant/servant-docs
# cabal sandbox add-source deps/servant-snap/deps/servant/servant-client
# cabal sandbox add-source deps/servant-snap/deps/servant/servant-blaze
# cabal sandbox add-source deps/servant-snap/deps/servant/servant-foreign
# cabal sandbox add-source deps/servant-snap/deps/servant/servant-js
# cabal sandbox add-source deps/servant-snap/deps/servant/servant-lucid
# cabal sandbox add-source deps/servant-snap/deps/servant/servant-mock
# cabal sandbox add-source deps/servant-snap/deps/servant/servant-server

cabal sandbox add-source deps/websockets
cabal sandbox add-source deps/websockets-snap

cabal sandbox add-source deps/servant-snap/

(cd backend && cabal sandbox init --sandbox=../.cabal-sandbox)

# Don't see a need for these now.  We probably don't need the cabal file in
# shared either.  I mainly put it there to make sure the directory got added
# in the initial commit.

# (cd frontend && cabal sandbox init --sandbox=../.cabal-sandbox)
# (cd shared && cabal sandbox init --sandbox=../.cabal-sandbox)
