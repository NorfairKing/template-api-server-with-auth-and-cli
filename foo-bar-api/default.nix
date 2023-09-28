{ mkDerivation, aeson, autodocodec, base, jose, lib, persistent
, servant, servant-auth, servant-auth-server, text, validity
, validity-text
}:
mkDerivation {
  pname = "foo-bar-api";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base jose persistent servant servant-auth
    servant-auth-server text validity validity-text
  ];
  homepage = "https://github.com/NorfairKing/foo-bar-api-cli-login#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
