{ mkDerivation, aeson, async, base, bytestring, Cabal, cassava
, containers, directory, filepath, generics-sop, ghc-prim, hint
, HUnit, indentation, integration, logfloat, math-functions
, monad-loops, mtl, mwc-random, parallel, parsec, pretty, primitive
, process, random, stdenv, tagged, template-haskell, text
, transformers, vector, zlib
}:
mkDerivation {
  pname = "hakaru";
  version = "0.2.0";
  src = ./.;
  buildDepends = [
    aeson async base bytestring Cabal cassava containers directory
    filepath generics-sop ghc-prim hint HUnit indentation integration
    logfloat math-functions monad-loops mtl mwc-random parallel parsec
    pretty primitive process random tagged template-haskell text
    transformers vector zlib
  ];
  testDepends = [
    base Cabal containers generics-sop ghc-prim hint HUnit integration
    logfloat math-functions monad-loops mtl mwc-random pretty primitive
    process random tagged template-haskell text transformers vector
  ];
  homepage = "http://indiana.edu/~ppaml/";
  description = "A probabilistic programming embedded DSL";
  license = stdenv.lib.licenses.bsd3;
}
