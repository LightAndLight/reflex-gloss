{ mkDerivation, base, dependent-sum, gloss, mtl, reflex, reflex-basic-host, stdenv
, transformers
}:
mkDerivation {
  pname = "reflex-gloss";
  version = "0.3";
  src = ./.;
  libraryHaskellDepends = [
    base dependent-sum gloss mtl reflex reflex-basic-host transformers
  ];
  homepage = "https://github.com/reflex-frp/reflex-gloss";
  description = "An reflex interface for gloss";
  license = stdenv.lib.licenses.bsd3;
}
