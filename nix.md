# Using SnapLayout for XMonad with NixOS

## Installing

### With Flakes

Add the following in the `inputs` section of your `flake.nix`:

```nix
snaplayout-for-xmonad = {
  url = "git+ssh://git@github.com/google/snaplayout-for-xmonad.git";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

(Note that the `inputs.nixpkgs.follows = "nixpkgs"` part is optional - see
https://nix.dev/manual/nix/2.24/command-ref/new-cli/nix3-flake#flake-inputs for
more details).

Also make sure you're capturing the `outputs` args:

```nix
outputs = { self, nixpkgs, ... }@inputs:
```

And passing them through in the `specialArgs` field of `config`:

```nix
specialArgs = {
  inherit inputs;
};
```

Finally, in your `configuration.nix`, receive the `inputs` arg:

```nix
{ config, lib, pkgs, inputs, ... }:
{
```

and then add something like this in your main config section:

```nix
  services.xserver.enable = true;
  services.xserver.windowManager.xmonad = {
    enableContribAndExtras = true;
    enable = true;
    extraPackages = haskellPackages: [
      inputs.snaplayout-for-xmonad.packages.${pkgs.system}.snaplayout-for-xmonad
    ];
  };
```

Note that after rebuilding NixOS, you may need to reboot before it'll start
actually working.

### Without Flakes

Add something like this to the top of your `configuration.nix`:

```nix
{ config, pkgs, ... }:
let
  snaplayout-for-xmonad = (import (fetchTarball {
    url = "https://github.com/google/snaplayout-for-xmonad/archive/refs/tags/v0.0.5.0.tar.gz";
    sha256 = "sha256:0kfxq585f8r9sijsf0mpabhi94260h7qs01h80gsl9za6p2vrlkr";
  })).outputs.packages.${builtins.currentSystem}.snaplayout-for-xmonad;
in
{
```

And then something like this in the main body:

```nix
  services.xserver.enable = true;
  services.xserver.windowManager.xmonad = {
    enableContribAndExtras = true;
    enable = true;
    extraPackages = haskellPackages: [
      snaplayout-for-xmonad
    ];
  };
```

Note that after rebuilding NixOS, you may need to reboot before it'll start
actually working.

## Updating Dependencies

Unfortunately, because of the way this whole setup works with NixOS, if you
update your system, you might end up in a situation where the Haskell packages
that the nixpkgs version of XMonad wants are different from the packages that
SnapLayout for XMonad wants. This'll result in them getting packaged with
separate versions of ghc, and XMonad will not be able to see SnapLayout for
XMonad.

If you see errors when you start XMonad about not being able to find SnapLayout
and FocusOnTop, this is probably what's happened. To verify, you can run `ls
/nix/store | grep ghc` and find the ones that look like
`<hash>-ghc-<version>-with-packages`. If there are multiple, then you can
browse around to try to find where the SnapLayout library is (or isn't) - for
example, your library may reside in
`...-ghc-9.6.6-with-packages/lib/ghc-9.6.6/lib/x86_64-linux-ghc-9.6.6/libHSsnaplayout-for-xmonad-0.0.5.0-...`.
If the SnapLayout library is not in the same `ghc` install as XMonad, then you
need to update the SnapLayout deps (and hope it still compiles).

You can do that as follows:

1. Remove the version specifications for all of the `build-depends` libraries
   in [snaplayout-for-xmonad.cabal](snaplayout-for-xmonad.cabal).
2. Update the SnapLayout for XMonad flake (`nix flake update`).
3. Make sure your main system flake is pointing to your version of the SnapLayout checkout.
4. Rebuilt NixOS.
5. Find the new versions of the dependencies by checking in the new
   `/nix/store/<hash>-ghc-<version>-with-packages` directory, and add those
   back to the `snaplayout-for-xmonad.cabal` file. This step is technically
   optional, but not doing it might sort of break the reproducible-build
   property of NixOS.
6. Reboot your system and see if it worked. Logging out may not be good enough.
