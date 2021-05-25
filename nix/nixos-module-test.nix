{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
}:
let
  foo-bar-production = import (./nixos-module.nix) { envname = "production"; fooBarPackages = pkgs.fooBarPackages; };
  home-manager = import (sources.home-manager + "/nixos/default.nix");
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
      home-manager = {
        useGlobalPkgs = true;
        users.testuser = { pkgs, ... }: {
          imports = [
            ./home-manager-module.nix
          ];
          xdg.enable = true;
          home.stateVersion = "20.09";
          programs.foo-bar = {
            enable = true;
            fooBarPackages = pkgs.fooBarPackages;
            sync = {
              enable = true;
              server-url = "localhost:${builtins.toString port}";
              username = "testuser";
              password = "testpassword";
            };
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
