import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

main :: IO ()
main = xmonad $ def
  `additionalKeys`
    [ ((noModMask, xK_Super_L), spawn "rofi -show run")
    ]

