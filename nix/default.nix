{ mkDerivation
, stdenv

  # haskell deps
, base
, bytes
, instant-generics
, tasty
, tasty-quickcheck
}:

mkDerivation {
  pname = "instant-bytes";
  version = "0.1.0.1";
  homepage = "https://github.com/k0001/instant-bytes";
  description = "Generic Aeson instances through instant-generics";
  license = stdenv.lib.licenses.bsd3;
  src = ../.;
  isLibrary = true;
  isExecutable = false;
  doHaddock = true;
  buildDepends = [base bytes instant-generics tasty tasty-quickcheck];
}