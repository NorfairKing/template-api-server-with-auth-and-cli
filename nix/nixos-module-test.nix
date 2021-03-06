{ pkgs ? import ./pkgs.nix }:
let
  foo-bar-production = import (./nixos-module.nix) { envname = "production"; };
  home-manager = import (
    builtins.fetchTarball {
      url = "https://github.com/rycee/home-manager/archive/472ca211cac604efdf621337067a237be9df389e.tar.gz";
      sha256 = "sha256:1gbfsnd7zsxwqryxd4r6jz9sgdz6ghlkapws1cdxshrbxlwhqad1";
    } + "/nixos/default.nix"
  );

  port = 8001;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "foo-bar-module-test";
    machine = {
      imports = [
        foo-bar-production
        home-manager
      ];
      services.foo-bar.production = {
        enable = true;
        api-server = {
          enable = true;
          inherit port;
        };
      };
      users.users.testuser.isNormalUser = true;
      home-manager.users.testuser = { pkgs, ... }: {
        imports = [
          ./home-manager-module.nix
        ];
        xdg.enable = true;
        home.stateVersion = "20.09";
        programs.foo-bar = {
          enable = true;
          sync = {
            enable = true;
            server-url = "localhost:${builtins.toString port}";
            username = "testuser";
            password = "testpassword";
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      machine.wait_for_unit("multi-user.target")

      machine.wait_for_open_port(${builtins.toString port})
      machine.succeed("curl localhost:${builtins.toString port}")

      machine.wait_for_unit("home-manager-testuser.service")


      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"


      machine.succeed(su("testuser", "cat ~/.config/foo-bar/config.yaml"))

      machine.succeed(su("testuser", "foo-bar register"))
      machine.succeed(su("testuser", "foo-bar login"))
      machine.succeed(su("testuser", "foo-bar greet"))
    '';
  }
)
