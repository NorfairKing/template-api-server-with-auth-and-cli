{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.foo-bar;

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };

  toYamlFile = pkgs.callPackage ./to-yaml.nix { };

in
{
  options =
    {
      programs.foo-bar =
        {
          enable = mkEnableOption "Foo/Bar cli";
          fooBarPackages =
            mkOption {
              description = "The fooBarPackages attribute defined in the nix/overlay.nix file in the foo-bar repository.";
              default = (import ./pkgs.nix { }).fooBarPackages;
            };
          config =
            mkOption {
              description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
              default = { };
            };
          username =
            mkOption {
              type = types.nullOr types.str;
              example = "user";
              default = null;
              description =
                "The username to use when logging into the API server";
            };
          password =
            mkOption {
              type = types.nullOr types.str;
              example = "hunter12";
              default = null;
              description =
                "The password to use when logging into the API server";
            };
          server-url =
            mkOption {
              type = types.nullOr types.str;
              description = "Url to the api server";
              example = "https://api.foo-bar.com";
              default = null;
            };
        };
    };
  config =
    let
      nullOrOption =
        name: opt: optionalAttrs (!builtins.isNull opt) { "${name}" = opt; };

      fooBarConfig =
        mergeListRecursively [
          (nullOrOption "server-url" cfg.server-url)
          (nullOrOption "username" cfg.username)
          (nullOrOption "password" cfg.password)
          cfg.config
        ];

      # Convert the config file to pretty yaml, for readability.
      # The keys will not be in the "right" order but that's fine.
      fooBarConfigFile = toYamlFile "foo-bar-config" fooBarConfig;

      packages =
        [
          cfg.fooBarPackages.foo-bar-cli
        ];

    in
    mkIf cfg.enable {
      xdg = {
        configFile."foo-bar/config.yaml".source = fooBarConfigFile;
      };
      home.packages = packages;
    };
}
