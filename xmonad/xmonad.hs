import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

main :: IO ()
main = xmonad $ def
    { modMask = mod4Mask -- Rebind Mod to Super Key
    }
  `additionalKeysP`
    [ ("M-p", spawn "rofi -show run")
    ]

