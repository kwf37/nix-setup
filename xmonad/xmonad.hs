import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Run

import Graphics.X11.ExtraTypes.XF86

myPolybar :: StatusBarConfig
myPolybar = statusBarProp "polybar example" (pure xmobarPP)

main :: IO ()
main =
    xmonad 
    $ ewmhFullscreen
    $ ewmh
    $ withSB myPolybar
    $ docks def {
        layoutHook = avoidStruts (Tall 1 (3/100) (1/2) ||| Full)
    }
  `additionalKeys`
    [ ((noModMask, xK_Super_L), spawn "rofi -show run")
    , ((noModMask, xF86XK_MonBrightnessUp), spawn "light -A 5; ~/.config/scripts/spawn_conky_brightness.sh")
    , ((noModMask, xF86XK_MonBrightnessDown), spawn "light -U 5; ~/.config/scripts/spawn_conky_brightness.sh")
    ]

