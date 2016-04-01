#!/bin/sh

chmod 644 ~/tmp/hsnippet-deploy/static/hsnippet.js
cat dist/build/hsnippet-frontend/hsnippet-frontend.jsexe/out.js dist/build/hsnippet-frontend/hsnippet-frontend.jsexe/runmain.js > hsnippet.js
cp hsnippet.js ~/tmp/hsnippet-deploy/static
