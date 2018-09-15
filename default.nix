(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    src = ./.;
  };

  shells = {
    ghc = ["src"];
  };

  overrides = self: super: {
    gloss-rendering = self.callPackage (import ./nix/gloss-rendering.nix) {};
    gloss = self.callPackage (import ./nix/gloss.nix) {};
    gloss-juicy = self.callPackage (import ./nix/gloss-juicy.nix) {};
    
    reflex-gloss = self.callCabal2nix "reflex-gloss" (pkgs.fetchFromGitHub {
      owner = "lightandlight";
      repo = "reflex-gloss";
      rev = "69248b7b0ffb6bddb1c161bf85d023d23893617d";
      sha256 = "1s5ap46pqz6rik69pkbsv2vnb55ibnqlpln3q6r148s54x4ia694";
    }) {};
  };
})
