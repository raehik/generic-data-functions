{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default = self'.packages.generic-data-functions;
        haskellProjects.ghc96 = import ./haskell-flake-ghc96.nix pkgs;
        haskellProjects.default = {
          #basePackages = config.haskellProjects.ghc96.outputs.finalPackages;
          devShell = {
            tools = hp: {
              ghcid = null; # broken on GHC 9.6? fsnotify
              hlint = null; # broken on GHC 9.6?
              haskell-language-server = null; # TAKES AGES TO BUILD FFS
            };
          };
        };
      };
    };
}
