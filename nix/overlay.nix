final: previous:
with final.haskell.lib;

{
  fooBarPackages =
    let
      fooBarPkg =
        name:
        doBenchmark (
          addBuildDepend
            (
              failOnAllWarnings (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
                )
              )
            )
            (final.haskellPackages.autoexporter)
        );
      fooBarPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (fooBarPkg name);
      fooBarPkgWithOwnComp = name: fooBarPkgWithComp name name;

    in
    {
      "foo-bar-api" = fooBarPkg "foo-bar-api";
      "foo-bar-api-gen" = fooBarPkg "foo-bar-api-gen";
      "foo-bar-api-server" = fooBarPkgWithOwnComp "foo-bar-api-server";
      "foo-bar-api-server-gen" = fooBarPkg "foo-bar-api-server-gen";
      "foo-bar-cli" = fooBarPkgWithComp "foo-bar" "foo-bar-cli";
      "foo-bar-client" = fooBarPkg "foo-bar-client";
    };

  fooBarRelease =
    final.symlinkJoin {
      name = "foo-bar-release";
      paths = final.lib.attrValues final.fooBarPackages;
    };


  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                let
                  # envparse
                  envparseRepo =
                    final.fetchFromGitHub {
                      owner = "supki";
                      repo = "envparse";
                      rev = "de5944fb09e9d941fafa35c0f05446af348e7b4d";
                      sha256 =
                        "sha256:0piljyzplj3bjylnxqfl4zpc3vc88i9fjhsj06bk7xj48dv3jg3b";
                    };
                  envparsePkg =
                    dontCheck (
                      self.callCabal2nix "envparse" envparseRepo { }
                    );
                in
                final.fooBarPackages // {
                  envparse = envparsePkg;
                  genvalidity-appendful = appendfulPkg "genvalidity-appendful";
                }
            );
      }
    );
}
