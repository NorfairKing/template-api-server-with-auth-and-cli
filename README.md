# API Server with CLI Template

This is a template implementation of an API server with an example command-line tool to go along with it.
It features complete option parsing, like in [template-optparse](https://github.com/NorfairKing/template-optparse), a command-line tool like in [template-cli](https://github.com/NorfairKing/template-cli) as well as  handlers, testing and best-practices.

* Haskell code for an api-server
* Per-route integration tests for the api-server
* Haskell code for an accompanying command-line tool
* Per-command integration test for the cli tool
* Option parsing & Option parsing tests for both the server and the cli tool
* Nix build
* CI
  * Stack-based CI
  * Nix-based CI
* Pre-commit hooks

## License

This template is **not** free to use.
See https://template.cs-syd.eu/template/NorfairKing/template-api-server-with-auth-and-cli for more information.

Copyright (c) 2020 Tom Sydney Kerckhove.

All Rights Reserved.

## Instructions

To use this template in a new project, choose the name for your project, for example `shelter`.
Then use [template-filler](https://github.com/NorfairKing/template-filler) to use the template, like this:

```
template-filler --source /path/to/this/template-cli --destination /path/to/your/shelter --find Foobar --replace Shelter
```

### Template overview

TODO

### OptParse

The option parsing is based on [the option parsing template](https://github.com/NorfairKing/template-optparse).
It is included in this template so you will not need to also buy the option parsing template.

For more information about how to use the option parsing, follow the instructions in `template-cli/src/Foobar/Cli/OptParse.hs`.

### Nix build

If you don't need a nix build, remove these files:

```
rm -rf *.nix nix .github/workflows/nix.yaml
```

In `nix/nixpkgs-version.nix`, we pin a `nixpkgs` commit.
In `nix/pkgs.nix` we define our own 'version' of the `nixpkgs` by adding our own overlays.
The project overlay is defined in `nix/overlay.nix`.

See the instructions in `nix/overlay.nix` for more details.

### CI

CI is set up for both a stack build and a nix build.
See `.github/workflows` for more details.

The stack build should "just work".

For the nix build to work, there is a manual step that you need to go through:
First, make a cachix cache at cachix.org.
Put its name in the right places within `.github/workflows/nix.yaml`.
Then put its signing key in the 'Secrets' part of your repository on github.

### Workflow examples

TODO
