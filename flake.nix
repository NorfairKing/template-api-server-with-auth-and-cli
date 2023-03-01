{
  description = "fooBar";
  nixConfig = {
    extra-substituters = "https://foobar.cachix.org";
    extra-trusted-public-keys = "foobar.cachix.org-1:srabhQPgZR0EO+bOppsCWbesHOgk8ABakPL8D1h5wOU=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.11";
    home-manager.url = "github:nix-community/home-manager?ref=release-22.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=flake";
    sydtest.flake = false;
    appendful.url = "github:NorfairKing/appendful?ref=flake";
    appendful.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    , appendful
    , dekking
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (appendful + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;
      mkNixosModule = import ./nix/nixos-module.nix {
        inherit (pkgs.fooBarReleasePackages) foo-bar-api-server;
      };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.fooBarRelease;
      checks.${system} = {
        release = self.packages.${system}.default;
        shell = self.devShells.${system}.default;
        nixos-module-test = import ./nix/nixos-module-test.nix {
          inherit (pkgs) nixosTest;
          home-manager = home-manager.nixosModules.home-manager;
          foo-bar-nixos-module-factory = self.nixosModuleFactories.${system}.default;
          foo-bar-home-manager-module = self.homeManagerModules.${system}.default;
        };
        coverage-report = pkgs.dekking.makeCoverageReport {
          name = "test-coverage-report";
          packages = [
            "foo-bar-api"
            "foo-bar-api-gen"
            "foo-bar-api-server"
            "foo-bar-api-server-gen"
            "foo-bar-cli"
            "foo-bar-client"
          ];
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "fooBar-shell";
        packages = p: builtins.attrValues p.fooBarPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          niv
          zlib
          cabal-install
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nixosModules.${system}.default = mkNixosModule { envname = "production"; };
      nixosModuleFactories.${system}.default = mkNixosModule;
      homeManagerModules.${system}.default = import ./nix/home-manager-module.nix {
        fooBarReleasePackages = pkgs.fooBarReleasePackages;
      };
    };
}
