#!/bin/sh

# A little script to speed up the setup of a deploy environment from the
# output of nix-build.

mkdir log
mkdir -p userbuild/snippets
cp -R --no-preserve=mode userbuild-template/* userbuild
chmod +x userbuild/run-build.sh

cp --no-preserve=mode devel.cfg.template devel.cfg
