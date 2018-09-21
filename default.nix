{ reflex-platform ? import ./nix/reflex-platform.nix } :
let

  haskellPackages = reflex-platform.ghc.override {
    overrides = self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
      gloss-rendering = self.callPackage (import ./nix/gloss-rendering.nix) {};
      gloss = self.callPackage (import ./nix/gloss.nix) {};
    };
  };

  reflex-gloss = haskellPackages.callPackage ./reflex-gloss.nix {
    reflex-basic-host =
      import (reflex-platform.nixpkgs.fetchFromGitHub {
        owner = "dalaing";
        repo = "reflex-basic-host";
        rev = "a4b188267994f2d0dcef7c9bb5da12c881d4d15d";
        sha256 = "0kaccv1fljw045r582zaql3qms98ym30r756cmq1hq7lh8ijzd2d";
      }) { inherit reflex-platform; };
  };
in
  reflex-gloss
