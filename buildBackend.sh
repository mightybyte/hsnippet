#!/bin/sh

cd backend
. ~/.nix-profile/etc/profile.d/nix.sh
nix-build
