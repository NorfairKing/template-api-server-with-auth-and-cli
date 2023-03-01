{ mkDerivation, base, foo-bar-api, lib, servant, servant-auth
, servant-auth-client, servant-client, servant-client-core, text
, validity, validity-text
}:
mkDerivation {
  pname = "foo-bar-client";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base foo-bar-api servant servant-auth servant-auth-client
    servant-client servant-client-core text validity validity-text
  ];
  homepage = "https://github.com/NorfairKing/foo-bar#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
