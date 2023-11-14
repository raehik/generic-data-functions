{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default = self'.packages.ghc96-generic-data-functions;
        haskellProjects.ghc94.basePackages = pkgs.haskell.packages.ghc94;
        haskellProjects.ghc96 = {
          # 2023-11-14: GHC 9.6 base package set is borked
          # PR: https://github.com/NixOS/nixpkgs/pull/267477
          basePackages = pkgs.haskell.packages.ghc96.override {
            overrides = self: super: {
              fgl = self.fgl_5_8_2_0;
              th-desugar = self.th-desugar_1_16;
            };
          };
        };
      };
    };
}
