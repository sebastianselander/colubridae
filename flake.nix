{
  description = "Brainlette the Javalette compiler";

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
            # Look into managing specific versions of dependencies
          };
        };
        devShells = {
          default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              haskell.compiler.ghc982
              (haskell-language-server.override
              { supportedGhcVersions = [ "982" ]; })
            ];
          };
        };
      });
}
