#!/bin/sh

# A little script to speed up the setup of a deploy environment from the
# output of nix-build.

# Usage
# setup-env.sh /nix/store/ynd62cqd1mlfmfgf7vwbm235p6zw05mb-hsnippet /tmp/hsnippet

cp -R $1/* $2
cd $2
mkdir log
mkdir -p userbuild/snippets
cp -R userbuild-template/* userbuild

cp devel.cfg.template devel.cfg
