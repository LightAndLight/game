{ mkDerivation, base, bmp, bytestring, fetchgit, gloss, JuicyPixels
, stdenv, vector
}:
mkDerivation {
  pname = "gloss-juicy";
  version = "0.2.3";
  src = fetchgit {
    url = "https://github.com/hpacheco/gloss-juicy";
    sha256 = "1g52b7a40f8lni8nd6180j4mb657r6g28ywxqfw5hqbyx0grydsp";
    rev = "e8fb82f2e335afa7b1f5f792c44ce61ef0cb4664";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bmp bytestring gloss JuicyPixels vector
  ];
  executableHaskellDepends = [
    base bmp bytestring gloss JuicyPixels vector
  ];
  homepage = "http://github.com/hpacheco/gloss-juicy";
  description = "Load any image supported by Juicy.Pixels in your gloss application";
  license = stdenv.lib.licenses.bsd3;
}
