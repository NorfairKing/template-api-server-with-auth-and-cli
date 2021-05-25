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
          enable = mkEnableOption "Foo/Bar cli and syncing";
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
          sync =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Foo/Bar syncing";
                        server-url =
                          mkOption {
                            type = types.str;
                            example = "api.foo-bar.cs-syd.eu";
                            description = "The url of the sync server";
                          };
                        username =
                          mkOption {
                            type = types.str;
                            example = "syd";
                            description =
                              "The username to use when logging into the sync server";
                          };
                        password =
                          mkOption {
                            type = types.str;
                            example = "hunter12";
                            description =
                              "The password to use when logging into the sync server";
                          };
                      };
                  }
                );
            };
        };
    };
  config =
    let
      syncConfig = optionalAttrs (cfg.sync.enable or false) {
        server-url = cfg.sync.server-url;
        username = cfg.sync.username;
        password = cfg.sync.password;
      };

      syncFooBarName = "sync-foo-bar";
      syncFooBarService =
        {
          Unit =
            {
              Description = "Sync foo-bar";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "sync-foo-bar-service-ExecStart"
                  ''
                    exec ${cfg.fooBarPackages.foo-bar-cli}/bin/foo-bar sync
                  ''}";
              Type = "oneshot";
            };
        };
      syncFooBarTimer =
        {
          Unit =
            {
              Description = "Sync foo-bar every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "daily";
              Persistent = true;
              Unit = "${syncFooBarName}.service";
            };
        };

      fooBarConfig =
        mergeListRecursively [
          syncConfig
          cfg.config
        ];

      # Convert the config file to pretty yaml, for readability.
      # The keys will not be in the "right" order but that's fine.
      fooBarConfigFile = toYamlFile "foo-bar-config" fooBarConfig;

      services =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncFooBarName}" = syncFooBarService;
          }
        );
      timers =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncFooBarName}" = syncFooBarTimer;
          }
        );
      packages =
        [
          cfg.fooBarPackages.foo-bar-cli
        ];


    in
    mkIf cfg.enable {
      xdg = {
        configFile."foo-bar/config.yaml".source = fooBarConfigFile;
      };
      systemd.user =
        {
          startServices = true;
          services = services;
          timers = timers;
        };
      home.packages = packages;
    };
}
