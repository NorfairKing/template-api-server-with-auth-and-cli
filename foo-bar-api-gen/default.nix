{ mkDerivation, base, foo-bar-api, genvalidity, genvalidity-hspec
, genvalidity-text, hspec, lib, text
}:
mkDerivation {
  pname = "foo-bar-api-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base foo-bar-api genvalidity genvalidity-text text
  ];
  testHaskellDepends = [ base foo-bar-api genvalidity-hspec hspec ];
  homepage = "https://github.com/NorfairKing/foo-bar-api-cli-login#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
