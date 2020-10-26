{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.foobar;


in
{
  options =
    {
      programs.foobar =
        {
          enable = mkEnableOption "Foobar cli and syncing";
          decks =
            mkOption {
              type = types.listOf types.str;
              example = [ "~/decks" ];
              default = [];
              description = "Where to find the decks to study";
            };
          extraConfig =
            mkOption {
              type = types.str;
              description = "Extra contents for the config file";
              default = "";
            };
          sync =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Foobar syncing";
                        server-url =
                          mkOption {
                            type = types.str;
                            example = "api.foobar.cs-syd.eu";
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
      foobarPkgs = (import ./pkgs.nix).foobarPackages;
      configContents = cfg: ''
        
decks: ${builtins.toJSON cfg.decks}
${cfg.extraConfig}

      '';
      syncConfigContents = syncCfg:
        optionalString (syncCfg.enable or false) ''

server-url: "${cfg.sync.server-url}"
username: "${cfg.sync.username}"
password: "${cfg.sync.password}"

      '';


      syncFoobarName = "sync-foobar";
      syncFoobarService =
        {
          Unit =
            {
              Description = "Sync foobar";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "sync-foobar-service-ExecStart"
                  ''
                    exec ${foobarPkgs.foobar-cli}/bin/foobar sync
                  ''}";
              Type = "oneshot";
            };
        };
      syncFoobarTimer =
        {
          Unit =
            {
              Description = "Sync foobar every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "daily";
              Persistent = true;
              Unit = "${syncFoobarName}.service";
            };
        };

      foobarConfigContents =
        concatStringsSep "\n" [
          (configContents cfg)
          (syncConfigContents cfg.sync)
        ];

      services =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncFoobarName}" = syncFoobarService;
          }
        );
      timers =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncFoobarName}" = syncFoobarTimer;
          }
        );
      packages =
        [
          foobarPkgs.foobar-cli
          foobarPkgs.foobar-tui
        ];


    in
      mkIf cfg.enable {
        xdg = {
          configFile."foobar/config.yaml".text = foobarConfigContents;
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
