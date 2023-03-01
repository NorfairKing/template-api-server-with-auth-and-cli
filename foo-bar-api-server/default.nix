{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, bytestring, envparse, foo-bar-api, jose, lib, monad-logger, mtl
, optparse-applicative, password, password-instances, path, path-io
, persistent, persistent-sqlite, persistent-template
, servant-auth-server, servant-server, text, time, validity
, validity-persistent, wai, warp, yaml
}:
mkDerivation {
  pname = "foo-bar-api-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base bytestring envparse
    foo-bar-api jose monad-logger mtl optparse-applicative password
    password-instances path path-io persistent persistent-sqlite
    persistent-template servant-auth-server servant-server text time
    validity validity-persistent wai warp yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/foo-bar#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "foo-bar-api-server";
}
