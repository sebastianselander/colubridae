{
  description = "Colubridae programming language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        haskell-packages = nixpkgs.legacyPackages.${system}.haskell.packages;
        ghcVersion = "ghc982";
        pkgs = import nixpkgs { inherit system; };
      in {
        packages = {
          default = haskell-packages.${ghcVersion}.developPackage {
            root = ./.;
            withHoogle = true;
          };
        };
        devShells = {
          default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              haskell.compiler.ghc98
              (haskell-language-server.override {
                supportedGhcVersions = [ "98" ];
              })
              haskellPackages.containers
              haskellPackages.relude
              haskellPackages.megaparsec
              haskellPackages.syb
              haskellPackages.parser-combinators
            ];
          };
        };
      });
}
