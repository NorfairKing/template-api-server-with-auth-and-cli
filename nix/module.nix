{ envname }:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.foobar."${envname}";
  concatAttrs = attrList: fold (x: y: x // y) {} attrList;
in
{
  options.services.foobar."${envname}" =
    {
      enable = mkEnableOption "Foobar Service";
      api-server =
        mkOption {
          type =
            types.submodule {
              options =
                {
                  enable = mkEnableOption "Foobar API Server";
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
                      example = "api.foobar.cs-syd.eu";
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
                            enable = mkEnableOption "Foobar API Server Local Backup Service";
                            backup-dir = mkOption {
                              type = types.str;
                              example = "backup/api-server";
                              default = "backup/api-server";
                              description = "The directory to store backups in, relative to the /www/foobar/${envname} directory or absolute";
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
      web-server =
        mkOption {
          type =
            types.submodule {
              options =
                {
                  enable = mkEnableOption "Foobar Web Server";
                  api-url =
                    mkOption {
                      type = types.str;
                      example = "api.foobar.cs-syd.eu.eu";
                      description = "The url for the api to use";
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
                      example = "foobar.cs-syd.eu";
                      description = "The host to serve web requests on";
                    };
                  port =
                    mkOption {
                      type = types.int;
                      example = 8002;
                      description = "The port to serve web requests on";
                    };
                  google-analytics-tracking =
                    mkOption {
                      type = types.nullOr types.str;
                      example = "XX-XXXXXXXX-XX";
                      default = null;
                      description = "The Google analytics tracking code";
                    };
                  google-search-console-verification =
                    mkOption {
                      type = types.nullOr types.str;
                      example = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
                      default = null;
                      description = "The Google search console verification code";
                    };
                };
            };
        };
    };
  config =
    let
      foobarPkgs = (import ./pkgs.nix).foobarPackages;
      working-dir = "/www/foobar/${envname}/";
      # The docs server
      api-server-working-dir = working-dir + "api-server/";
      api-server-database-file = api-server-working-dir + "foobar-server-database.sqlite3";
      # The api server
      api-server-service =
        with cfg.api-server;
        optionalAttrs enable {
          "foobar-api-server-${envname}" = {
            description = "Foobar API Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "FOOBAR_API_SERVER_LOG_LEVEL" =
                  "${builtins.toString log-level}";
                "FOOBAR_API_SERVER_PORT" =
                  "${builtins.toString port}";
                "FOOBAR_API_SERVER_DATABASE" = api-server-database-file;
              };
            script =
              ''
                mkdir -p "${api-server-working-dir}"
                cd ${api-server-working-dir};
                ${foobarPkgs.foobar-api-server}/bin/foobar-api-server
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

        optionalAttrs enable {
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
              "foobar-api-server-local-backup-${envname}" = {
                description = "Backup foobar-api-server database locally for ${envname}";
                wantedBy = [];
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
              "foobar-api-server-local-backup-${envname}" = {
                description = "Backup foobar-api-server database locally for ${envname} every twelve hours.";
                wantedBy = [ "timers.target" ];
                timerConfig = {
                  OnCalendar = "00/12:00";
                  Persistent = true;
                };
              };
            }
          )
        );

      # The web server
      web-server-working-dir = working-dir + "web-server/";
      web-server-data-dir = web-server-working-dir + "web-server/";
      web-server-service =
        with cfg.web-server;
        optionalAttrs enable {
          "foobar-web-server-${envname}" = {
            description = "Foobar web server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "FOOBAR_WEB_SERVER_API_URL" = "${api-url}";
                "FOOBAR_WEB_SERVER_LOG_LEVEL" = "${builtins.toString log-level}";
                "FOOBAR_WEB_SERVER_PORT" = "${builtins.toString port}";
                "FOOBAR_WEB_SERVER_DATA_DIR" = web-server-data-dir;
              } // optionalAttrs (!builtins.isNull google-analytics-tracking) {
                "FOOBAR_WEB_SERVER_GOOGLE_ANALYTICS_TRACKING" = "${google-analytics-tracking}";
              } // optionalAttrs (!builtins.isNull google-search-console-verification) {
                "FOOBAR_WEB_SERVER_GOOGLE_SEARCH_CONSOLE_VERIFICATION" = "${google-search-console-verification}";
              };
            script =
              ''
                mkdir -p "${web-server-working-dir}"
                ${foobarPkgs.foobar-web-server}/bin/foobar-web-server \
                  serve
              '';
            serviceConfig =
              {
                WorkingDirectory = web-server-working-dir;
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
      web-server-host =
        with cfg.web-server;

        optionalAttrs enable {
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
    in
      mkIf cfg.enable {
        systemd.services =
          concatAttrs [
            api-server-service
            web-server-service
            local-backup-service
          ];
        systemd.timers =
          concatAttrs [
            local-backup-timer
          ];
        networking.firewall.allowedTCPPorts = builtins.concatLists [
          (optional cfg.api-server.enable cfg.api-server.port)
          (optional cfg.web-server.enable cfg.web-server.port)
        ];
        services.nginx.virtualHosts =
          concatAttrs [
            api-server-host
            web-server-host
          ];
      };
}
