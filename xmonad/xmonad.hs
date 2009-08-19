import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Themes
import XMonad.Layout.TabBarDecoration

-- bound in xinitrc to alt_r
myModMask = mod3Mask

main = do
    h <- spawnPipe ("dzen2")
    xmonad $ defaultConfig
        { modMask = myModMask
        , workspaces = ["emacs", "web", "irc", "sh", "min"]
        , focusFollowsMouse = False
        , terminal = "urxvt"
        , borderWidth = 2
        , logHook = dynamicLogWithPP byorgeyPP { ppOutput = hPutStrLn h }
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        }
