{ sources ? import ./sources.nix
}:
let
  pkgsv = import sources.nixpkgs;
  hastoryPkgs =
    pkgsv {
      overlays =
        [
          (import (sources.validity + "/nix/overlay.nix"))
          (import (sources.yamlparse-applicative + "/nix/overlay.nix"))
          (import (sources.safe-coloured-text + "/nix/overlay.nix"))
          (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
          (import ./overlay.nix)
        ];
      config.allowUnfree = true;
    };
in
hastoryPkgs
