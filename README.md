# SnapLayout for XMonad

**This is not an officially supported Google product**

SnapLayout ([SnapLayout.hs](SnapLayout.hs)) is an [XMonad](https://xmonad.org/) [LayoutClass](https://hackage.haskell.org/package/xmonad-0.17.0/docs/XMonad-Core.html#t:LayoutClass) that lets the user snap windows to particular edges of the screen, at 1/2 screen width or height.

Usage:

You can use this module with the following in your `~/.xmonad/xmonad.hs`:

```haskell
import SnapLayout
```

Then edit your `layoutHook` by adding the SnapLayout:

```haskell
myLayout = SnapLayout def ||| Full ||| etc..
main = xmonad def { layoutHook = myLayout }
```

For more detailed instructions on editing the `layoutHook` see:

[XMonad.Doc.Extending#Editing_the_layout_hook](https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Doc-Extending.html#Editing_the_layout_hook)

In the key-bindings, do something like:

```haskell
, ((modm .|. controlMask, xK_KP_Left ), withFocused (sendMessage . Snap SnapLayout.Left))
, ((modm .|. controlMask, xK_KP_Right), withFocused (sendMessage . Snap SnapLayout.Right))
, ((modm .|. controlMask, xK_KP_Up   ), withFocused (sendMessage . Snap Top))
, ((modm .|. controlMask, xK_KP_Down ), withFocused (sendMessage . Snap Bottom))
...
```

For detailed instruction on editing the key binding see:

[XMonad.Doc.Extending#Editing_key_bindings](https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Doc-Extending.html#Editing_key_bindings)

## Installing Dependencies

Using [Cabal](https://www.haskell.org/cabal/) (recommended):

```
cabal new-install --lib xmonad xmonad-contrib data-default
```

## Future Plans

* support snapping to corners of the screen as well as sides
* use alternative layout (maybe [Tall](https://hackage.haskell.org/package/xmonad-0.17.0/docs/XMonad-Layout.html#t:Tall), [Tabbed](https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Layout-Tabbed.html), or [Grid](https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Layout-Grid.html)?) for windows that aren't snapped
* allow manual resizing of snapped windows
* allow users to cycle through multiple standard sizes (1/2 screen, 1/3 screen, and 2/3 screen, for example)
  * if we start using non-1/2 sizes, then maybe also options for windows snapped to "middle cells" rather than just screen edges?
* possibly a single-window full-screen mode
* a way to "unsnap" a window
* stack overlapping windows in a better order (maybe last touched on top?)

## Contributing

See [CONTRIBUTING](./CONTRIBUTING.md) for more.

## License

See [LICENSE](./LICENSE) for more.