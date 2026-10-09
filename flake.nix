{
  description = "Lambda Calculus-like thing in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc9103;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
        packageName = "llama-hs";
      in
      {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self { };

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            haskellPackages.haskell-language-server
            haskellPackages.cabal-install
            haskellPackages.cabal-gild
            haskellPackages.hlint
            haskellPackages.fourmolu
          ];
          withHoogle = true;
          inputsFrom = builtins.attrValues self.packages.${system};
        };
        devShell = self.devShells.${system}.default;
      });
}
