import XMonad hiding (Tall) -- use the one in HintedTile
import qualified XMonad.StackSet as W

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP,removeKeysP)
import XMonad.Util.Themes
import XMonad.Util.Loggers

import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.HintedTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.TabBarDecoration

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import System.Exit
import System.IO
import Data.Char (toLower)
import Data.Ratio ((%))

-- bound in xinitrc to alt_r
myModMask = mod3Mask

-- Log hook
myLogHook h = dynamicLogWithPP $ defaultPP
   { ppOutput           = \s -> hPutStrLn h (" " ++ s)
   , ppCurrent          = dzenColor "#000" "steel blue" . wrap " " " "
   , ppHidden           = dzenColor "grey" "#000" . wrap "" "."
   , ppHiddenNoWindows  = dzenColor "grey" "#000"
   , ppLayout = map toLower
   , ppTitle  = shorten 50
   , ppExtras = [ logCmd "sh -c 'echo `whoami`@`uname -n`'" ]
   , ppWsSep  = " "
   , ppSep    = dzenColor "steel blue" "#000" " :: "
   , ppOrder  = \(ws:l:t:exs) -> exs++[ws,l,t]
   }

-- Layout
myLayout = avoidStruts $ layoutHints $ smartBorders $ layoutOrder
  where layoutOrder = Full ||| tiled Tall ||| Grid ||| Circle
        tiled       = HintedTile nmaster delta ratio TopLeft
        nmaster     = 1
        delta       = 3/100
        ratio       = 3/5

-- Key bindings
spawnOnWs ws command = (windows $ W.greedyView ws) >> spawn command
myAdditionalKeys =
    [ -- run anything
      ("M-S-x", spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

      -- predetermined locations
    , ("M-x e",   spawnOnWs "sauce" "emacsclient -c")
    , ("M-x S-e", spawnOnWs "sauce" "emacs")
    , ("M-x f",   spawnOnWs "web" "firefox")
    , ("M-x i",   spawnOnWs "irc" "emacsclient -c -e '(irc)'")
    , ("M-x m",   spawnOnWs "music" "ario")
    , ("M-x v",   spawnOnWs "music" "urxvt -e alsamixer")

      -- volume control
    , ("M-v m",   spawn "aumix -v 0")
    , ("M-v S-m", spawn "aumix -v 100")
    , ("M-=",     spawn "aumix -v +5")
    , ("M-+",     spawn "aumix -v +5")
    , ("M--",     spawn "aumix -v -5")
    , ("M-_",     spawn "aumix -v -5")

      -- gtfo
    , ("M-q", spawn "killall dzen2; killall conky" >> restart "xmonad" True)
    , ("M-S-<F12>", io (exitWith ExitSuccess))
    ]
myRemovedKeys = ["M-S-q", "M-p"]

-- Bars
myDzenCommand = "dzen2 -bg black -fg grey -fn '-*-dina-medium-r-*-*-*-*-*-*-*-*-*-*'"
myXmonadBar   = myDzenCommand ++ " -ta l -w 750"
myStatusBar   = "conky | " ++ myDzenCommand ++ " -ta r -x 750"

-- Float things that should be
myManageHook = composeAll
    [ title     =? "Downloads" --> doFloat
    , className =? "Ario"      --> doF (W.shift "music")
    , manageDocks ] <+> manageHook defaultConfig

-- XConfig
myConfig xmonadBar = defaultConfig
    { modMask            = myModMask
    , workspaces         = ["web", "sauce", "irc", "music", "log"]
    , focusFollowsMouse  = False
    , terminal           = "urxvt"
    , borderWidth        = 1
    , normalBorderColor  = "#555753"
    , focusedBorderColor = "steel blue"
    , logHook            = myLogHook xmonadBar
    , manageHook         = myManageHook
    , layoutHook         = myLayout
    }

main = do
    xmonadBar <- spawnPipe myXmonadBar
    statusBar <- spawnPipe myStatusBar
    xmonad $ myConfig xmonadBar
        `additionalKeysP` myAdditionalKeys
        `removeKeysP` myRemovedKeys
