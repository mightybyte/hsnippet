{ reflex-platform ? import ./deps/reflex-platform {} }:
let nixpkgs = reflex-platform.nixpkgs;
in nixpkgs.stdenv.mkDerivation (rec {
  name = "hsnippet";
  snaplets = ./app/snaplets;
  static = ./app/static;
  app = ./app;

  deps = ./deps;
  lib = ./hsnippet-lib;
  deploy = ./deploy;
  userbuild = ./userbuild;
  backend = import ./backend { nixpkgs = reflex-platform.nixpkgs; };
  frontend = import ./frontend { inherit reflex-platform; };
  develCfgTemplate = ./app/devel.cfg.template;
  buildEnv = (import ./userbuild/default.nix {}).env.nativeBuildInputs;
  myNixPkgs = nixpkgs.path;
  builder = builtins.toFile "builder.sh" ''
    source "$stdenv/setup"

    set -x

    mkdir -p "$out"

    ln -s "$myNixPkgs" "$out/nixpkgs"
    cp -r --no-preserve=mode "$static" "$out/static"
    cp -r --no-preserve=mode "$snaplets" "$out/snaplets"
    ln -s "$static" "$out/static"
    ln -s "$userbuild" "$out/userbuild-template"
    ln -s "$lib" "$out/hsnippet-lib"
    mkdir -p "$out/deps"
    ln -s "$deps/diagrams-reflex" "$out/deps/diagrams-reflex"
    ln -s "$deps/reflex-dom-contrib" "$out/deps/reflex-dom-contrib"
    ln -s "$deps/reflex-platform" "$out/deps/reflex-platform"
    echo "$buildEnv" >"$out/buildEnv"

    rm -f "$out/static/rts.js"
    rm -f "$out/static/lib.js"
    rm -f "$out/static/hsnippet.js"
    ln -s "$frontend/bin/hsnippet-frontend.jsexe/rts.js" "$out/static"
    ln -s "$frontend/bin/hsnippet-frontend.jsexe/lib.js" "$out/static"
    cat "$frontend/bin/hsnippet-frontend.jsexe/out.js" "$frontend/bin/hsnippet-frontend.jsexe/runmain.js" > "$out/static/hsnippet.js"

    # closure-compiler -O ADVANCED --js_output_file="$out/static/rts.js" "$frontend/bin/hsnippet-frontend.jsexe/rts.js"
    # closure-compiler -O ADVANCED --js_output_file="$out/static/lib.js" "$frontend/bin/hsnippet-frontend.jsexe/lib.js"
    # cat "$frontend/bin/hsnippet-frontend.jsexe/out.js" "$frontend/bin/hsnippet-frontend.jsexe/runmain.js" > "$out/static/hsnippet-unoptimized.js"
    # closure-compiler -O ADVANCED --js_output_file="$out/static/hsnippet.js" "$out/static/hsnippet-unoptimized.js"

    mkdir "$out/bin"
    ln -s "$backend/bin/main" "$out/bin"
    ln -s "$backend/bin/build-snippet" "$out/bin"
    ln -s "$deploy/setup-env.sh" "$out/bin/setup-env.sh"

    ln -s "$develCfgTemplate" "$out/devel.cfg.template"
  '';
  buildInputs = with nixpkgs; [
    closurecompiler
  ];
})

