{ envname, fooBarPackages ? (import ./pkgs.nix { }).fooBarPackages }:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.foo-bar."${envname}";

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };

  toYamlFile = pkgs.callPackage ./to-yaml.nix { };

in
{
  options.services.foo-bar."${envname}" =
    {
      enable = mkEnableOption "Foo/Bar Service";
      api-server =
        mkOption {
          type =
            types.submodule {
              options =
                {
                  enable = mkEnableOption "Foo/Bar API Server";
                  config = mkOption {
                    default = { };
                    description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
                  };
                  log-level =
                    mkOption {
                      type = types.str;
                      example = "Debug";
                      default = "Warn";
                      description = "The log level to use";
                    };
                  hosts =
                    mkOption {
                      type = types.listOf (types.str);
                      default = [ ];
                      example = "api.foo-bar.cs-syd.eu";
                      description = "The host to serve api requests on";
                    };
                  port =
                    mkOption {
                      type = types.int;
                      example = 8001;
                      description = "The port to serve api requests on";
                    };
                  local-backup =
                    mkOption {
                      type = types.nullOr (
                        types.submodule {
                          options = {
                            enable = mkEnableOption "Foo/Bar API Server Local Backup Service";
                            backup-dir = mkOption {
                              type = types.str;
                              example = "backup/api-server";
                              default = "backup/api-server";
                              description = "The directory to store backups in, relative to the /www/foo-bar/${envname} directory or absolute";
                            };
                          };
                        }
                      );
                      default = null;
                    };
                };
            };
          default = null;
        };
    };
  config =
    let
      working-dir = "/www/foo-bar/${envname}/";
      attrOrNull = name: value: optionalAttrs (!builtins.isNull value) { "${name}" = value; };
      api-server-config = with cfg.api-server; mergeListRecursively [
        (attrOrNull "port" port)
        (attrOrNull "log-level" log-level)
        cfg.api-server.config
      ];
      api-server-config-file = toYamlFile "foo-bar-api-server-config" api-server-config;
      # The docs server
      api-server-working-dir = working-dir + "api-server/";
      api-server-database-file = api-server-working-dir + "foo-bar-server-database.sqlite3";
      # The api server
      api-server-service =
        with cfg.api-server;
        optionalAttrs enable {
          "foo-bar-api-server-${envname}" = {
            description = "Foo/Bar API Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "FOO_BAR_API_SERVER_CONFIG_FILE" = "${api-server-config-file}";
              };
            script =
              ''
                mkdir -p "${api-server-working-dir}"
                cd ${api-server-working-dir};
                ${fooBarPackages.foo-bar-api-server}/bin/foo-bar-api-server
              '';
            serviceConfig =
              {
                Restart = "always";
                RestartSec = 1;
                Nice = 15;
              };
            unitConfig =
              {
                StartLimitIntervalSec = 0;
                # ensure Restart=always is always honoured
              };
          };
        };
      api-server-host =
        with cfg.api-server;

        optionalAttrs (enable && hosts != [ ]) {
          "${head hosts}" =
            {
              enableACME = true;
              forceSSL = true;
              locations."/" = {
                proxyPass = "http://localhost:${builtins.toString port}";
                # Just to make sure we don't run into 413 errors on big syncs
                extraConfig = ''
                  client_max_body_size 0;
                '';
              };
              serverAliases = tail hosts;
            };
        };

      # Local backup
      local-backup-service =
        optionalAttrs (cfg.api-server.enable or false) (
          optionalAttrs (cfg.api-server.local-backup.enable or false) (
            with cfg.api-server.local-backup;
            {
              "foo-bar-api-server-local-backup-${envname}" = {
                description = "Backup foo-bar-api-server database locally for ${envname}";
                wantedBy = [ ];
                script =
                  ''
                    mkdir -p ${backup-dir}
                    file="${backup-dir}/''$(date +%F_%T).db"
                    ${pkgs.sqlite}/bin/sqlite3 ${api-server-database-file} ".backup ''${file}"
                  '';
                serviceConfig = {
                  WorkingDirectory = working-dir;
                  Type = "oneshot";
                };
              };
            }
          )
        );
      local-backup-timer =
        optionalAttrs (cfg.api-server.enable or false) (
          optionalAttrs (cfg.api-server.local-backup.enable or false) (
            with cfg.api-server.local-backup;
            {
              "foo-bar-api-server-local-backup-${envname}" = {
                description = "Backup foo-bar-api-server database locally for ${envname} every twelve hours.";
                wantedBy = [ "timers.target" ];
                timerConfig = {
                  OnCalendar = "00/12:00";
                  Persistent = true;
                };
              };
            }
          )
        );
    in
    mkIf cfg.enable {
      systemd.services =
        mergeListRecursively [
          api-server-service
          local-backup-service
        ];
      systemd.timers =
        mergeListRecursively [
          local-backup-timer
        ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional cfg.api-server.enable cfg.api-server.port)
      ];
      services.nginx.virtualHosts =
        mergeListRecursively [
          api-server-host
        ];
    };
}
