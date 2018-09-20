let
  reflex-platform = import ./reflex-platform {};
in
reflex-platform.project ({ pkgs, ... }: {
  packages = {
    src = ./.;
  };

  shells = {
    ghc = ["src"];
  };

  overrides = self: super: {
    # exception-transformers =
      # self.callCabal2nix "exception-transformers" (pkgs.fetchFromGitHub {
        # owner = "mainland";
        # repo = "exception-transformers";
        # rev = "991d5110439cdded7a8caa2d4f324ff8c107e82e";
        # sha256 = "1darlxxlzdslzirdhx966m3nvhflylqga6qrr820kskbm539s7vj";
      # }) {};
    # HUnit = self.callHackage "HUnit" "1.5.0.0" {};
    gloss-rendering = self.callPackage (import ./nix/gloss-rendering.nix) {};
    gloss = self.callPackage (import ./nix/gloss.nix) {};
    gloss-juicy = self.callPackage (import ./nix/gloss-juicy.nix) {};
    reflex-gloss =
      import ../reflex-gloss { inherit reflex-platform; };
    # reflex-gloss = self.callCabal2nix "reflex-gloss" (pkgs.fetchFromGitHub {
      # owner = "lightandlight";
      # repo = "reflex-gloss";
      # rev = "59fa34dd382415a72f6ddce115d76b94d9f6f2e0";
      # sha256 = "1kz3zd8hfq36zmicqz3m7qwlbq804lgjamji79g1a50davwxj0b9";
    # }) {};
  };
})
