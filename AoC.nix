{ mkDerivation, base, containers, papa, stdenv, text }:
mkDerivation {
  pname = "AoC";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers papa text ];
  license = stdenv.lib.licenses.bsd3;
}
