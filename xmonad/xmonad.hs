import XMonad hiding (Tall) -- use the one in HintedTile
import qualified XMonad.StackSet as W

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP,removeKeysP,additionalMouseBindings)
import XMonad.Util.Themes
import XMonad.Util.Loggers

import XMonad.Layout.Circle
import XMonad.Layout.LayoutHints
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Actions.Volume

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

-- Workspaces
myWorkspaces = ["web", "sauce", "irc", "music", "5", "6", "7", "8", "9"]

-- Layouts
myLayout = avoidStruts
           $ smartBorders
           $ Full ||| tiled ||| Circle
  where tiled    = named "tile" $ hinted (ResizableTall 1 (3/100) (3/5) [])
        hinted l = layoutHints l

-- Key bindings
spawnOnWs ws command = (windows $ W.greedyView ws) >> spawn command

myAdditionalKeys =
    [ -- run anything
      ("M-S-x", spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

      -- predetermined locations
    , ("M-x e",   spawnOnWs "sauce" "emacsclient -c")
    , ("M-x S-e", spawnOnWs "sauce" "emacs")
    , ("M-x w",   spawnOnWs "web"   "chromium")
    , ("M-x i",   spawnOnWs "irc"   "emacsclient -c -e '(irc)'")
    , ("M-x m",   spawnOnWs "music" "banshee")
    , ("M-x v",   spawnOnWs "music" "urxvt -e alsamixer")

      -- don't move
    , ("M-u M-x e", spawn "emacsclient -c")
    , ("M-u M-x w", spawn "chromium")

      -- volume control
    , ("<XF86AudioMute>",        toggleMute >> return ())
    , ("<XF86AudioRaiseVolume>", raiseVolume 5 >> return ())
    , ("<XF86AudioLowerVolume>", lowerVolume 5 >> return ())

      -- window management
    , ("M-S-h", sendMessage MirrorShrink)
    , ("M-S-l", sendMessage MirrorExpand)

      -- gtfo
    , ("M-q", spawn "killall dzen2; killall conky" >> restart "xmonad" True)
    , ("M-S-<F12>", io (exitWith ExitSuccess))
    ]
myRemovedKeys = ["M-S-q", "M-p"]

-- mod3 (alt_r) is my mouse hand, so allow mod1 for resizing float windows
myAdditionalMouseBindings =
    [ ((mod1Mask, button1), (\w -> focus w >> mouseMoveWindow w
                                           >> windows W.shiftMaster))
    , ((mod1Mask, button3), (\w -> focus w >> mouseResizeWindow w
                                           >> windows W.shiftMaster))
    ]

-- Bars
myDzenCommand = "dzen2 -bg black -fg grey -fn '-*-dina-medium-r-*-*-*-*-*-*-*-*-*-*'"
myXmonadBar   = myDzenCommand ++ " -ta l -w 750"
myStatusBar   = "conky | " ++ myDzenCommand ++ " -ta r -x 750"

-- Float things that should be
myManageHook = composeAll
    [ title     =? "Downloads" --> doFloat
    , manageDocks ] <+> manageHook defaultConfig

-- XConfig
myConfig xmonadBar = defaultConfig
    { modMask            = myModMask
    , workspaces         = myWorkspaces
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
        `additionalMouseBindings` myAdditionalMouseBindings
