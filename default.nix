{ mkDerivation
, base
, containers
, network
, stdenv
, text
}:

mkDerivation
{
  pname = "pickle";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers network text ];
  description = "Instant StatsD in Haskell";
  license = stdenv.lib.licenses.mit;
}
