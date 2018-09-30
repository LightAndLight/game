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
    JuicyPixels-extra = self.callHackage "JuicyPixels-extra" "0.3.0" {};
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
      rev = "6c3b9ec218199cd684096fa35d1601e2ef234fa8";
      sha256 = "13b42px5lxc4lqxm7sppfriz51k72rqvb75n4ygwnhzd3dzjvf3k";
    }) { inherit reflex-platform; };
  };
})
