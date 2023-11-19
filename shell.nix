{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { config = { }; overlays = [ ]; }
}:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
    HUnit
    base
    containers
    data-default
    xmonad
    xmonad-contrib
  ]);
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
  ];
}
