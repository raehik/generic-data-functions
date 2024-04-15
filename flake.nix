# TODO
# * better devshell name overriding. clumsy because we can't access the
#   derivation being used (because it's auto-grabbed). really just wanna change
#   `ghc-shell-for` to `ghcXY` and keep the `-${pname}-${version}`!
# * honestly maybe I move away from haskell-flake...? it's weird

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    type-level-bytestrings.url   = "github:raehik/type-level-bytestrings";
    type-level-bytestrings.flake = false;
  };
  outputs = inputs:
  let
    # simple devshell for non-dev compilers: really just want `cabal repl`
    nondevDevShell = compiler: {
      mkShellArgs.name = "${compiler}-generic-data-functions";
      hoogle = false;
      tools = _: {
        hlint = null;
        haskell-language-server = null;
        ghcid = null;
      };
    };
  in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default  = self'.packages.ghc96-generic-data-functions;
        devShells.default = self'.devShells.ghc96;
        haskellProjects.ghc98 = {
          basePackages = pkgs.haskell.packages.ghc98;
          packages.type-level-bytestrings.source = inputs.type-level-bytestrings;
          devShell = nondevDevShell "ghc98";
        };
        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          packages.type-level-bytestrings.source = inputs.type-level-bytestrings;
          devShell.mkShellArgs.name = "ghc96-generic-data-functions";
        };
        haskellProjects.ghc94 = {
          basePackages = pkgs.haskell.packages.ghc94;
          packages.type-level-bytestrings.source = inputs.type-level-bytestrings;
          devShell = nondevDevShell "ghc94";
        };
        haskellProjects.ghc92 = {
          basePackages = pkgs.haskell.packages.ghc92;
          packages.type-level-bytestrings.source = inputs.type-level-bytestrings;
          devShell = nondevDevShell "ghc92";
        };
      };
    };
}
