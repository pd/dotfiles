import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Themes
import XMonad.Util.Loggers
import XMonad.Layout.TabBarDecoration

-- bound in xinitrc to alt_r
myModMask = mod3Mask

myDzenPP h = defaultPP
   { ppOutput = hPutStrLn h
   , ppCurrent = dzenColor "#855c1b" "#000" . wrap "<" "*>"
   , ppHidden = dzenColor "grey" "#000" . wrap "" "*"
   , ppHiddenNoWindows = dzenColor "grey" "#000"
   , ppExtras = [ date "%a %b %d  %I:%M %p"
                , loadAvg
                , battery
                ]
   , ppWsSep = " "
   , ppSep = " | "
   , ppTitle = shorten 45
   , ppOrder = \(ws:l:t:exs) -> [t,l,ws] ++ exs
   }

myDzenFont = "'-*-terminus-*-*-*-*-14-*-*-*-*-*-iso8859'"
myDzenCommand = "dzen2 -bg '#000' -fg 'grey' -fn " ++ myDzenFont

main = do
    dzen <- spawnPipe myDzenCommand
    xmonad $ defaultConfig
        { modMask = myModMask
        , workspaces = ["web", "sauce", "irc", "sh", "min"]
        , focusFollowsMouse = False
        , terminal = "urxvt"
        , borderWidth = 1
        , normalBorderColor = "#555753"
        , focusedBorderColor = "#729fcf"
        , logHook = dynamicLogWithPP (myDzenPP dzen)
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        }
