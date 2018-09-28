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
    gloss-rendering = self.callPackage (import ./nix/gloss-rendering.nix) {};
    gloss = self.callPackage (import ./nix/gloss.nix) {};
    gloss-juicy = self.callPackage (import ./nix/gloss-juicy.nix) {};
    reflex-basic-host = import (pkgs.fetchFromGitHub {
      owner = "dalaing";
      repo = "reflex-basic-host";
      rev = "a4b188267994f2d0dcef7c9bb5da12c881d4d15d";
      sha256 = "0kaccv1fljw045r582zaql3qms98ym30r756cmq1hq7lh8ijzd2d";
    }) { inherit reflex-platform; };
    # reflex-gloss = import ../reflex-gloss { inherit reflex-platform; };
    reflex-gloss = import (pkgs.fetchFromGitHub {
      owner = "lightandlight";
      repo = "reflex-gloss";
      rev = "ec11dd09a7bd3f6c8f2c72f7b7c19900aadb44ae";
      sha256 = "1w8ig8l9w40bw0kvwh5rhwn3fy0fggl1kn6la437v998xgmskh4h";
    }) { inherit reflex-platform; };
  };
})
