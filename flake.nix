{
  inputs = {
    utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      utils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ghcVersion = "ghc9103";
        haskell = pkgs.haskell.packages.${ghcVersion};
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.cabal-install
            haskell.ghc
            haskell.haskell-language-server
            haskell.fourmolu
            haskell.stack
          ];
        };
      }
    );
}
