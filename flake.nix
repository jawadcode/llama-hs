# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0
{
  description = "Lambda Calculus-like thing in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};

      haskellPackages = pkgs.haskell.packages.ghc9103;

      jailbreakUnbreak = pkg:
        pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: {meta = {};}));

      packageName = "llama-hs";
    in {
      packages.${packageName} =
        haskellPackages.callCabal2nix packageName self {
        };

      packages.default = self.packages.${system}.${packageName};
      defaultPackage = self.packages.${system}.default;

      devShells.default = pkgs.mkShell {
        buildInputs = [
          haskellPackages.haskell-language-server # you must build it with your ghc to work
          # haskellPackages.hls-cabal-plugin
          haskellPackages.cabal-install
        ];
        inputsFrom = builtins.attrValues self.packages.${system};
      };
      devShell = self.devShells.${system}.default;
    });
}
