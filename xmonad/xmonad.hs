import XMonad hiding (Tall) -- use the one in HintedTile
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP,removeKeysP)
import XMonad.Util.Themes
import XMonad.Util.Loggers

import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.HintedTile
import XMonad.Layout.IM
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

-- Bars
myDzenCommand = "dzen2 -bg black -fg grey -fn '-*-dina-medium-r-*-*-*-*-*-*-*-*-*-*'"
myXmonadBar   = myDzenCommand ++ " -ta l -w 750"
myStatusBar   = "conky | " ++ myDzenCommand ++ " -ta r -x 750"

-- Layout
myLayout =
    onWorkspace "sauce" fullLayout $
    onWorkspace "irc" fullLayout $
    onWorkspace "im" imLayout $
    standardLayout
  where standardLayout = tiled Tall ||| fullLayout ||| Grid ||| Circle
        fullLayout = layoutHints(noBorders Full)
        imLayout   = withIM (1%5) (Role "buddy_list") $ tiled Tall
        tiled      = HintedTile nmaster delta ratio TopLeft
        nmaster    = 1
        delta      = 3/100
        ratio      = 3/5

-- Key bindings
myAdditionalKeys =
                 [ ("M-x e", spawn "emacsclient -c")
                 , ("M-x S-e", spawn "emacs")
                 , ("M-x f", spawn "firefox")
                 , ("M-x i", spawn "emacsclient -c -e '(irc)'")
                 , ("M-x v", spawn "urxvt -e alsamixer")
                 , ("M-v m", spawn "aumix -v 100")
                 , ("M-v S-m", spawn "aumix -v 0")
                 , ("M-=", spawn "aumix -v +2")
                 , ("M-+", spawn "aumix -v +2")
                 , ("M--", spawn "aumix -v -2")
                 , ("M-_", spawn "aumix -v -2")
                 , ("M-S-<F12>", io (exitWith ExitSuccess))
                 , ("M-q", spawn "killall dzen2; killall conky" >> restart "xmonad" True)
                 ]
myRemovedKeys = ["M-S-q"]

-- XConfig
myConfig xmonadBar = defaultConfig
    { modMask = myModMask
    , workspaces = ["web", "sauce", "irc", "im", "sh", "min"]
    , focusFollowsMouse = False
    , terminal = "urxvt"
    , borderWidth = 1
    , normalBorderColor = "#555753"
    , focusedBorderColor = "steel blue"
    , logHook = myLogHook xmonadBar
    , manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts $ myLayout
    }

main = do
    xmonadBar <- spawnPipe myXmonadBar
    statusBar <- spawnPipe myStatusBar
    xmonad $ myConfig xmonadBar
        `additionalKeysP` myAdditionalKeys
        `removeKeysP` myRemovedKeys
