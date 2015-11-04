{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
with import "${nixpkgs.path}/pkgs/development/haskell-modules/lib.nix" { pkgs=nixpkgs; };
let cabal2nix = src: nixpkgs.runCommand "nixified-cabal" {
      buildCommand = ''
        cabal2nix file://"${builtins.filterSource (path: type: path != ".git") src}" > $out
      '';
      buildInputs = [
        nixpkgs.cabal2nix
      ];

      # This stuff is necessary when the cabal file includes unicode characters
      ${if !nixpkgs.stdenv.isDarwin then "LOCALE_ARCHIVE" else null} = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
      ${if !nixpkgs.stdenv.isDarwin then "LC_ALL" else null} = "en_US.UTF-8";
    } "";
in (nixpkgs.pkgs.haskell.packages.${compiler}.override {
  overrides = self: super: {
    # Fixes
    QuickCheck = dontCheck super.QuickCheck;
    aeson = dontCheck (overrideCabal super.aeson (drv: { version = "0.9.0.1"; sha256 = "1g7qdq7zpyvqwmh4sfhizqpb51cg24lrcj9vq5msz8k896y7vfcj"; }));
    attoparsec = dontCheck (overrideCabal super.attoparsec (drv: { version = "0.13.0.1"; sha256 = "0cprkr7bl4lrr80pz8mryb4rbfwdgpsrl7g0fbcaybhl8p5hm26f"; }));
    aws = dontCheck super.aws;
    blaze-builder = dontCheck (overrideCabal super.blaze-builder (drv: { version = "0.4.0.1"; sha256 = "1id3w33x9f7q5m3xpggmvzw03bkp94bpfyz81625bldqgf3yqdn1"; }));
    blaze-markup = dontCheck (overrideCabal super.blaze-markup (drv: { version = "0.7.0.3"; sha256 = "080vlhd8dwjxrma4bb524lh8gxs5lm3xh122icy6lnnyipla0s9y"; }));
    blaze-html = dontCheck (overrideCabal super.blaze-html (drv: { version = "0.8.1.1"; sha256 = "1dnw50kh0s405cg9i2y4a8awanhj3bqzk21jwgfza65kcjby7lpq"; }));
    contravariant = addBuildDepend super.contravariant self.tagged;
    cookie = dontCheck super.cookie;
    crypto-pubkey = dontCheck super.crypto-pubkey;
    errors = dontCheck (overrideCabal super.errors (drv: { version = "2.0.1"; sha256 = "1w4pr6hbd731in6rcpxyn06h1zkjd1fnvzr5k2rn9zgsm7j8amby"; }));
    fsnotify = addBuildDepend (dontCheck (overrideCabal super.fsnotify (drv: { version = "0.2.1"; sha256 = "0asl313a52qx2w6dw25g845683xsl840bwjh118nkwi5v1xipkzb"; }))) self.unix-compat;
    #ghc-prim = dontCheck super.ghc-prim;
    hashable = dontCheck super.hashable;
    hmatrix = addBuildDepend (super.hmatrix.override { inherit (pkgs) blas; liblapack = pkgs.liblapack.override { shared = true; }; }) pkgs.gsl;
    lens = dontCheck super.lens;
    lifted-base = dontCheck super.lifted-base;
    mwc-random = dontCheck super.mwc-random;
    nats = addBuildDepend super.nats self.hashable;
    postgresql-simple = dontCheck super.postgresql-simple;
    reflection = dontHaddock (addBuildDepend super.reflection self.tagged);
    safecopy = dontCheck super.safecopy;
    snap-extras = dontCheck super.snap-extras;
    statistics = dontCheck super.statistics;
    text = dontCheck super.text;
    #unix-compat = dontCheck (overrideCabal super.unix-compat (drv: { version = "0.4.1.4"; sha256 = "0jxk7j5pz2kgfpqr4hznndjg31pqj5xg2qfc5308fcn9xyg1myps"; }));

    # Local packages
    reflex-dom-contrib = self.callPackage (cabal2nix ../deps/reflex-dom-contrib) {};
    reflex-platform = self.callPackage (cabal2nix ../deps/reflex-platform) {};
    snap-loader-dynamic = self.callPackage (cabal2nix ../deps/snap-loader-dynamic) {};
    snap-loader-static = self.callPackage (cabal2nix ../deps/snap-loader-static) {};
    heist = self.callPackage (cabal2nix ../deps/snap/deps/heist) {};
    io-streams-haproxy = dontCheck (self.callPackage (cabal2nix ../deps/snap/deps/io-streams-haproxy) {});
    io-streams = dontCheck (self.callPackage (cabal2nix ../deps/snap/deps/io-streams) {});
    snap-core = dontCheck (self.callPackage (cabal2nix ../deps/snap/deps/snap-core) {});
    snap-server = dontCheck (self.callPackage (cabal2nix ../deps/snap/deps/snap-server) {});
    xmlhtml = dontCheck (self.callPackage (cabal2nix ../deps/snap/deps/xmlhtml) {});
    snap = dontCheck (self.callPackage (cabal2nix ../deps/snap) {});
    snaplet-postgresql-simple = self.callPackage (cabal2nix ../deps/snaplet-postgresql-simple) {};
    websockets-snap = self.callPackage (cabal2nix ../deps/websockets-snap) {};
  };
}).callPackage ./packages.nix { }
