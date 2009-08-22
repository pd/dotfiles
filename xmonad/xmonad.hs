import XMonad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Themes
import XMonad.Util.Loggers

import XMonad.Layout.TabBarDecoration

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import System.IO
import Data.Char

-- bound in xinitrc to alt_r
myModMask = mod3Mask

-- Log hook
myLogHook h = dynamicLogWithPP $ defaultPP
   { ppOutput           = \s -> hPutStrLn h (" " ++ s)
   , ppCurrent          = dzenColor "#855c1b" "#000" . wrap "" "*"
   , ppHidden           = dzenColor "grey" "#000"
   , ppHiddenNoWindows  = dzenColor "grey" "#000"
   , ppLayout = map toLower . wrap "(" ")"
   , ppWsSep  = " "
   , ppSep    = "  "
   , ppTitle  = shorten 45
   , ppOrder  = \(ws:l:t:exs) -> [ws,l,t]++exs
   }

myDzenFont = "-*-liberation mono-medium-r-*-*-14-*-*-*-*-*-*-*"
myXMonadBarCommand = "dzen2 -ta l -bg '#000' -fg 'grey' -fn '" ++ myDzenFont ++ "'"

main = do
    xmonadBar <- spawnPipe myXMonadBarCommand
    xmonad $ defaultConfig
        { modMask = myModMask
        , workspaces = ["web", "sauce", "irc", "sh", "min"]
        , focusFollowsMouse = False
        , terminal = "urxvt"
        , borderWidth = 1
        , normalBorderColor = "#555753"
        , focusedBorderColor = "#729fcf"
        , logHook = myLogHook xmonadBar
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        }
