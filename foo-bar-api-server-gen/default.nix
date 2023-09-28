{ mkDerivation, base, bytestring, cookie, foo-bar-api
, foo-bar-api-gen, foo-bar-api-server, foo-bar-client
, genvalidity-hspec, hspec, http-client, http-types, lib
, monad-logger, persistent, persistent-sqlite, QuickCheck, servant
, servant-auth-client, servant-auth-server, servant-client, text
, warp
}:
mkDerivation {
  pname = "foo-bar-api-server-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cookie foo-bar-api foo-bar-api-gen
    foo-bar-api-server foo-bar-client genvalidity-hspec hspec
    http-client http-types monad-logger persistent persistent-sqlite
    QuickCheck servant servant-auth-client servant-auth-server
    servant-client text warp
  ];
  testHaskellDepends = [
    base bytestring cookie foo-bar-api foo-bar-api-gen
    foo-bar-api-server foo-bar-client genvalidity-hspec hspec
    http-client http-types monad-logger persistent persistent-sqlite
    QuickCheck servant servant-auth-client servant-auth-server
    servant-client text warp
  ];
  homepage = "https://github.com/NorfairKing/foo-bar#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
