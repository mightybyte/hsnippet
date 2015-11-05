#!/bin/sh

# A little script to speed up the setup of a deploy environment from the
# output of nix-build.

mkdir log
mkdir -p sandbox/snippets
cp --no-preserve=mode sandbox-template/* sandbox

cp --no-preserve=mode devel.cfg.template devel.cfg
