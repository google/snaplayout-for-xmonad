# SnapLayout for XMonad

**This is not an officially supported Google product**

SnapLayout ([SnapLayout.hs](SnapLayout.hs)) is an [XMonad](https://xmonad.org/) [LayoutClass](https://hackage.haskell.org/package/xmonad-0.17.0/docs/XMonad-Core.html#t:LayoutClass) that lets the user snap windows to particular edges or corners of the screen, at various fractions of screen width and/or height.

Usage:

You can use this module with the following in your `~/.xmonad/xmonad.hs`:

```haskell
import SnapLayout
```

Then edit your `layoutHook` by adding the SnapLayout:

```haskell
myLayout = snapLayout ||| Full ||| etc..
  where
    snapLayout :: SnapLayout a
    snapLayout = def
main = xmonad def { layoutHook = myLayout }
```

Or, to customize the fine adjustment increment:

```haskell
myLayout = snapLayout ||| Full ||| etc..
  where
    snapLayout :: SnapLayout a
    snapLayout = def { adjustmentDelta = 3/100 }
main = xmonad def { layoutHook = myLayout }
```

For more detailed instructions on editing the `layoutHook` see:

https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Doc-Extending.html#Editing_the_layout_hook

In the key-bindings, do something like:

```haskell
, ((modm .|. controlMask, xK_KP_End      ), withFocused (sendMessage . Snap BottomLeft))
, ((modm .|. controlMask, xK_KP_Down     ), withFocused (sendMessage . Snap Bottom))
, ((modm .|. controlMask, xK_KP_Page_Down), withFocused (sendMessage . Snap BottomRight))
, ((modm .|. controlMask, xK_KP_Left     ), withFocused (sendMessage . Snap SnapLayout.Left))
, ((modm .|. controlMask, xK_KP_Right    ), withFocused (sendMessage . Snap SnapLayout.Right))
, ((modm .|. controlMask, xK_KP_Home     ), withFocused (sendMessage . Snap TopLeft))
, ((modm .|. controlMask, xK_KP_Up       ), withFocused (sendMessage . Snap Top))
, ((modm .|. controlMask, xK_KP_Page_Up  ), withFocused (sendMessage . Snap TopRight))
, ((modm .|. controlMask, xK_KP_Insert   ), withFocused (sendMessage . Unsnap))

, ((modm .|. controlMask .|. shiftMask, xK_KP_End),
      withFocused (sendMessage . FineAdjustmentMessage BottomLeftAdjustment))
, ((modm .|. controlMask .|. shiftMask, xK_KP_Down),
      withFocused (sendMessage . FineAdjustmentMessage BottomAdjustment))
, ((modm .|. controlMask .|. shiftMask, xK_KP_Page_Down),
      withFocused (sendMessage . FineAdjustmentMessage BottomRightAdjustment))
, ((modm .|. controlMask .|. shiftMask, xK_KP_Left),
      withFocused (sendMessage . FineAdjustmentMessage LeftAdjustment))
, ((modm .|. controlMask .|. shiftMask, xK_KP_Right),
      withFocused (sendMessage . FineAdjustmentMessage RightAdjustment))
, ((modm .|. controlMask .|. shiftMask, xK_KP_Home),
      withFocused (sendMessage . FineAdjustmentMessage TopLeftAdjustment))
, ((modm .|. controlMask .|. shiftMask, xK_KP_Up),
      withFocused (sendMessage . FineAdjustmentMessage TopAdjustment))
, ((modm .|. controlMask .|. shiftMask, xK_KP_Page_Up),
      withFocused (sendMessage . FineAdjustmentMessage TopRightAdjustment))
, ((modm .|. controlMask .|. shiftMask, xK_KP_Insert), withFocused (sendMessage . Unadjust))
...
```

For detailed instruction on editing the key binding see:

https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Doc-Extending.html#Editing_key_bindings

## Installing Dependencies

Using [Cabal](https://www.haskell.org/cabal/) (recommended):

```
cabal new-install --lib xmonad xmonad-contrib data-default
```

## Future Plans

* use alternative layout (maybe [Tall](https://hackage.haskell.org/package/xmonad-0.17.0/docs/XMonad-Layout.html#t:Tall), [Tabbed](https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Layout-Tabbed.html), or [Grid](https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Layout-Grid.html)?) for windows that aren't snapped
* maybe add options for snapping windows snapped to "middle cells" rather than just screen edges? (for non-1/2-screen snaps)
* possibly a single-window full-screen mode

## Contributing

See [CONTRIBUTING](./CONTRIBUTING.md) for more.

## License

See [LICENSE](./LICENSE) for more.
