{ mkDerivation, base, containers, megaparsec, papa
, parser-combinators, stdenv, text, time
}:
mkDerivation {
  pname = "AoC";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers megaparsec papa parser-combinators text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
