import XMonad
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Run

myPolybar :: StatusBarConfig
myPolybar = statusBarProp "polybar example" (pure xmobarPP)

main :: IO ()
main =
    xmonad 
    $ withSB myPolybar
    $ def
  `additionalKeys`
    [ ((noModMask, xK_Super_L), spawn "rofi -show run")
    ]

