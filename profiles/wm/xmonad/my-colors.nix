{
  mkDerivation,
  base,
  lib,
}:
mkDerivation {
  pname = "my-colors";
  version = "0.1.0.0";
  src = ./my-colors;
  libraryHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
