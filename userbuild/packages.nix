{ pkgs, haskellPackages }: 
with import "${pkgs.path}/pkgs/development/haskell-modules/lib.nix" { inherit pkgs; };
haskellPackages.override {
  overrides = self: super: with self; {
    reflex-dom-contrib = self.callPackage ../deps/reflex-dom-contrib {};
    diagrams-reflex = self.callPackage ../deps/diagrams-reflex {};
    hsnippet-lib = self.callPackage ../hsnippet-lib {};
  };
}
