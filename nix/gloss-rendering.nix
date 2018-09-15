{ mkDerivation, base, bmp, bytestring, containers, fetchgit, GLUT
, OpenGL, stdenv
}:
mkDerivation {
  pname = "gloss-rendering";
  version = "1.13.0.0";
  src = fetchgit {
    url = "https://github.com/benl23x5/gloss.git";
    sha256 = "0rrplkfbi3ng8amq36i94wmq9k3kinp0ljp8305cckwkaryln41z";
    rev = "7b4cec721b99d675a3a19e9e055ee48c8aedbd7e";
  };
  postUnpack = "sourceRoot+=/gloss-rendering; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bmp bytestring containers GLUT OpenGL
  ];
  description = "Gloss picture data types and rendering functions";
  license = stdenv.lib.licenses.mit;
}
