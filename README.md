# CLI Template

This is a template implementation of a command-line tool.
It features complete option parsing, like in [template-optparse](https://github.com/NorfairKing/template-optparse) as well as command handlers, testing and best-practices

* Haskell code for a multi-command CLI tool
* Per-command integration test
* End-to-end test
* Option parsing & Option parsing tests
* Nix build
* CI
  * Stack-based CI
  * Nix-based CI
* Pre-commit hooks

## License

This template is **not** free to use.
See https://template.cs-syd.eu/template/NorfairKing/template-cli for more information.

Copyright (c) 2020 Tom Sydney Kerckhove.

All Rights Reserved.

## Instructions

To use this template in a new project, choose the name for your project, for example `shelter`.
Then use [template-filler](https://github.com/NorfairKing/template-filler) to use the template, like this:

```
template-filler --source /path/to/this/template-cli --destination /path/to/your/homeless-shelter --find Foobar --replace Shelter
```

### Template overview

There is a single haskell package in `foobar-cli`.
It contains the following structure:

- The entry point in `Foobar.Cli`
- Option parsing in `Foobar.Cli.OptParse`
- The (read-only) environment and related functions in `Foobar.Cli.Env`
- Individual commands in `Foobar.Cli.<command>`. There is an example in `Foobar.Cli.Commands.Greet`

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

#### Adding a command

1. Add the new command's option parsing in the `Foobar.Cli.OptParse` module according to the instructions within.

2. Add a `Foobar.Cli.Command.<CommandName>` module with a function as follows:
```
commandName :: CommandNameSettings -> C ()
```

3. Add a case to the `dispatch` function in `Foobar.Cli`.
4. Add tests in `Foobar.Cli.Command.<CommandName>Spec`.
