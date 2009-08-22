import XMonad hiding (Tall) -- use the one in HintedTile
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP,removeKeysP)
import XMonad.Util.Themes
import XMonad.Util.Loggers

import XMonad.Layout.HintedTile
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import System.Exit
import System.IO
import Data.Char (toLower)

-- bound in xinitrc to alt_r
myModMask = mod3Mask

-- Log hook
myLogHook h = dynamicLogWithPP $ defaultPP
   { ppOutput           = \s -> hPutStrLn h (" " ++ s)
   , ppCurrent          = dzenColor "#000" "#aaa" . wrap " " " "
   , ppHidden           = dzenColor "grey" "#000"
   , ppHiddenNoWindows  = dzenColor "grey" "#000"
   , ppLayout = map toLower . wrap "(" ")"
   , ppWsSep  = " "
   , ppSep    = "  "
   , ppOrder  = \(ws:l:t:exs) -> [ws,l,t]++exs
   }

-- Bars
myDzenCommand = "dzen2 -bg black -fg grey -fn '-*-liberation mono-medium-r-*-*-14-*-*-*-*-*-*-*'"
myXmonadBar   = myDzenCommand ++ " -ta l"
myStatusBar   = "conky | " ++ myDzenCommand ++ " -ta r -x 400"

-- Layout
standardLayout = tiled Tall ||| Full ||| Grid
    where tiled   = HintedTile nmaster delta ratio TopLeft
          nmaster = 1
          delta   = 3/100
          ratio   = 3/5
fullLayout = layoutHints(noBorders Full)
myLayout = onWorkspace "sauce" Full $
         onWorkspace "min" Grid $
         standardLayout

main = do
    xmonadBar <- spawnPipe myXmonadBar
    statusBar <- spawnPipe myStatusBar
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
        , layoutHook = avoidStruts $ myLayout
        }
        `additionalKeysP`
        [ ("M-x e", spawn "emacsclient -c")
        , ("M-x S-e", spawn "emacs")
        , ("M-x f", spawn "firefox")
        , ("M-S-<F12>", io (exitWith ExitSuccess))
        , ("M-q", spawn "killall dzen2; killall conky" >> restart "xmonad" True)
        ]
        `removeKeysP` ["M-S-q"]
