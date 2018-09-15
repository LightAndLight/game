{ mkDerivation, base, bmp, bytestring, containers, fetchgit
, ghc-prim, gloss-rendering, GLUT, OpenGL, stdenv
}:
mkDerivation {
  pname = "gloss";
  version = "1.13.0.1";
  src = fetchgit {
    url = "https://github.com/benl23x5/gloss.git";
    sha256 = "0rrplkfbi3ng8amq36i94wmq9k3kinp0ljp8305cckwkaryln41z";
    rev = "7b4cec721b99d675a3a19e9e055ee48c8aedbd7e";
  };
  postUnpack = "sourceRoot+=/gloss; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bmp bytestring containers ghc-prim gloss-rendering GLUT OpenGL
  ];
  homepage = "http://gloss.ouroborus.net";
  description = "Painless 2D vector graphics, animations and simulations";
  license = stdenv.lib.licenses.mit;
}
