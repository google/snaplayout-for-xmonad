{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-24.11;
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-compat, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        snaplayout-for-xmonad = pkgs.haskellPackages.developPackage {
          root = builtins.path { path = ./.; name = "snaplayout-for-xmonad"; };
        };
        snaplayout-for-xmonad-shell = pkgs.mkShell {
          inputsFrom = [ snaplayout-for-xmonad ];
          packages = with pkgs; [ cabal-install ];
        };
      in
      {
        packages = {
          inherit snaplayout-for-xmonad;
          default = snaplayout-for-xmonad;
        };
        devShells = {
          inherit snaplayout-for-xmonad-shell;
          default = snaplayout-for-xmonad-shell;
        };
      }
    );
}
