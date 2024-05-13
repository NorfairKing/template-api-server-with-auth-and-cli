{ nixosTest
, home-manager
, foo-bar-nixos-module-factory
, foo-bar-home-manager-module
}:
let
  foo-bar-production = foo-bar-nixos-module-factory {
    envname = "production";
  };
  port = 8001;
in
nixosTest ({
  name = "foo-bar-module-test";
  nodes = {
    server = {
      imports = [
        foo-bar-production
      ];
      services.foo-bar.production = {
        enable = true;
        api-server = {
          enable = true;
          inherit port;
          openFirewall = true;
        };
      };
    };
    client = {
      imports = [
        home-manager
      ];
      users.users.testuser.isNormalUser = true;
      home-manager = {
        useGlobalPkgs = true;
        users.testuser = {
          imports = [
            foo-bar-home-manager-module
          ];
          xdg.enable = true;
          home.stateVersion = "23.11";
          programs.foo-bar = {
            enable = true;
            username = "test";
            password = "test";
            server-url = "server:${builtins.toString port}";
          };
        };
      };
    };
  };
  testScript = ''
    from shlex import quote

    server.start()
    client.start()
    server.wait_for_unit("multi-user.target")
    client.wait_for_unit("multi-user.target")

    server.wait_for_open_port(${builtins.toString port})
    client.succeed("curl server:${builtins.toString port}")

    client.wait_for_unit("home-manager-testuser.service")


    def su(user, cmd):
        return f"su - {user} -c {quote(cmd)}"


    client.succeed(su("testuser", "cat ~/.config/foo-bar/config.yaml"))

    client.succeed(su("testuser", "foo-bar register"))
    client.succeed(su("testuser", "foo-bar login"))
    client.succeed(su("testuser", "foo-bar greet"))
  '';
}
)
