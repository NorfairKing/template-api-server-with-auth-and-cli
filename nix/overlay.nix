final: prev:
with final.haskell.lib;

{

  fooBarRelease =
    final.symlinkJoin {
      name = "foo-bar-release";
      paths = builtins.map justStaticExecutables (final.lib.attrValues final.haskellPackages.fooBarPackages);
    };


  haskellPackages =
    prev.haskellPackages.override (old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
        (
          self: super:
            let
              fooBarPackages =
                let
                  fooBarPkg = name: doBenchmark (
                    buildStrictly (
                      self.callPackage (../${name}) { }
                    )
                  );
                  fooBarPkgWithComp =
                    exeName: name:
                    generateOptparseApplicativeCompletion exeName (fooBarPkg name);
                  fooBarPkgWithOwnComp = name: fooBarPkgWithComp name name;

                in
                {
                  "foo-bar-api" = fooBarPkg "foo-bar-api";
                  "foo-bar-api-gen" = fooBarPkg "foo-bar-api-gen";
                  "foo-bar-api-server" = addBuildDepend (fooBarPkgWithOwnComp "foo-bar-api-server") final.haskellPackages.autoexporter;
                  "foo-bar-api-server-gen" = fooBarPkg "foo-bar-api-server-gen";
                  "foo-bar-cli" = addBuildDepend (fooBarPkgWithComp "foo-bar" "foo-bar-cli") final.haskellPackages.autoexporter;
                  "foo-bar-client" = fooBarPkg "foo-bar-client";
                };
            in
            fooBarPackages // { inherit fooBarPackages; }
        );
    });
}
