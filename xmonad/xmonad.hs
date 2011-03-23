import XMonad hiding (Tall) -- use the one in HintedTile
import qualified XMonad.StackSet as W

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP,removeKeysP,additionalMouseBindings)
import XMonad.Util.Themes
import XMonad.Util.Loggers

import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Magnifier as Mag
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

-- import XMonad.Actions.Volume
import XMonad.Actions.CycleWS

import System.Exit
import System.IO
import Data.Char (toLower)
import Data.Ratio ((%))

-- From ~/.xmonad/lib, ty pbrisbin
import Dzen

-- bound in xinitrc to alt_r
myModMask = mod3Mask

-- Main
main = do
    xmonadBar <- spawnDzen myLeftBar
    conkyBar  <- spawnToDzen "conky" myRightBar

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
       { modMask            = myModMask
       , workspaces         = myWorkspaces
       , focusFollowsMouse  = False
       , terminal           = "urxvt"
       , borderWidth        = 1
       , normalBorderColor  = "#444"
       , focusedBorderColor = "steel blue"
       , logHook            = myLogHook xmonadBar
       , manageHook         = myManageHook
       , layoutHook         = myLayout
       } `additionalKeysP` myAdditionalKeys
         `removeKeysP` myRemovedKeys
         `additionalMouseBindings` myAdditionalMouseBindings

-- Workspaces
myWorkspaces = ["web", "sauce", "irc", "im", "misc", "vm"]

-- Layouts
myLayout = avoidStruts $ smartBorders $
           onWorkspace "web" (webSplit ||| webWide ||| Full) $
           onWorkspace "im"  im $
                             (fullFirst)
    where
      -- default web layout, magnifies non-master windows a bit. nice when
      -- you pull an emacs frame to type something out while referencing the interwebs
      webSplit = named "wsplit" $ Mag.magnifiercz' 1.2 $ ResizableTall 1 (3/100) (60/100) []

      -- handy for webdev, browser atop a small shell/emacs frame:
      webWide  = named "wwide" $ Mirror $ ResizableTall 1 (3/100) (80/100) []

      -- pidgin ws
      im = gridIM (1%5) (Title "Buddy List")

      -- the generic layout order for less picky workspaces
      fullFirst = Full ||| Grid ||| tiled
      tiled     = named "tile" $ ResizableTall 1 (3/100) (3/5) []

myManageHook = mainManageHook <+> manageDocks
    where mainManageHook = composeAll $ concat
              [ [ title =? c     --> doCenterFloat | c <- myCenterFloats ]
              , [ className =? c --> doShift "web" | c <- myWebs ]
              , [ className =? c --> doShift "im"  | c <- myIMs ]
              , [ isDialog       --> doCenterFloat ]
              , [ className =? c --> doShift "vm"  | c <- myVM ]
              , [ className =? c --> doFloat       | c <- myVM ]
              , [ isFullscreen   --> doFullFloat ]
              ]
          myWebs = ["Chromium", "Firefox"]
          myIMs = ["Pidgin"]
          myCenterFloats = ["Downloads"]
          myVM = ["VirtualBox"]

-- Key bindings
myAdditionalKeys =
    [ -- run anything
      ("M-S-x", spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

      -- predetermined locations
    , ("M-x e",   spawnOnWs "sauce" "emacsclient -s main -c")
    , ("M-x M-e", spawnOnWs "sauce" "emacsclient -s main -c")
    , ("M-x S-e", spawnOnWs "sauce" "emacs")
    , ("M-x w",   spawnOnWs "web"   "chromium-dev")
    , ("M-x M-w", spawnOnWs "web"   "chromium")
    , ("M-x i",   spawnOnWs "irc"   "emacsclient -s irc -c --eval '(irc)'")

      -- don't move
    , ("M-u M-x e", spawn "emacsclient -s main -c")
    , ("M-u M-x w", spawn "chromium-dev")
    , ("M-u M-x M-w", spawn "chromium")

      -- window sizing
    , ("M-S-h", sendMessage MirrorShrink)
    , ("M-S-l", sendMessage MirrorExpand)

      -- workspace/screen maneuvering
    , ("M-<D>",   nextWS)
    , ("M-<U>",   prevWS)
    , ("M-<R>",   nextScreen)
    , ("M-<L>",   prevScreen)
    , ("M-S-<R>", shiftNextScreen)
    , ("M-S-<L>", shiftPrevScreen)

      -- gtfo
    , ("M-q", spawn "killall dzen2; killall conky" >> restart "xmonad" True)
    , ("M-S-<F8>", spawn "xlock -mode blank")
    , ("M-S-<F12>", io (exitWith ExitSuccess))
    ]
    ++
    [ (otherModMasks ++ "M-" ++ [key], action tag)
        | (tag, key) <- zip myWorkspaces "123456789"
        , (otherModMasks, action) <- [ ("", windows . W.view)
                                     , ("S-", windows . W.shift) ]
    ]

myRemovedKeys = ["M-S-q", "M-p"]

-- mod3 (alt_r) is my mouse hand, so allow mod1 for resizing float windows
myAdditionalMouseBindings =
    [ ((mod1Mask, button1), (\w -> focus w >> mouseMoveWindow w
                                           >> windows W.shiftMaster))
    , ((mod1Mask, button3), (\w -> focus w >> mouseResizeWindow w
                                           >> windows W.shiftMaster))
    ]

-- dzen bars
myLeftBar :: DzenConf
myLeftBar = defaultDzen
    { width    = Just $ Percent 45
    , font     = Just "Droid Sans Mono-10"
    , fg_color = Just "grey"
    , bg_color = Just "black"
    , exec     = []
    }

myRightBar :: DzenConf
myRightBar = myLeftBar
    { alignment  = Just RightAlign
    , x_position = Just $ Percent 45
    , width      = Just $ Percent 55
    }

-- Log hook
myLogHook h = dynamicLogWithPP $ defaultPP
   { ppOutput           = \s -> hPutStrLn h (" " ++ s)
   , ppCurrent          = dzenColor "#000" "steel blue" . wrap " " " "
   , ppHidden           = dzenColor "grey" "#000"
   , ppHiddenNoWindows  = dzenColor "#666" "#000"
   , ppUrgent = dzenColor "red" "#000" . dzenStrip
   , ppLayout = map toLower
   , ppTitle  = shorten 80
   , ppExtras = [ logCmd "sh -c 'echo `whoami`@`uname -n`'" ]
   , ppWsSep  = " "
   , ppSep    = dzenColor "steel blue" "#000" " :: "
   , ppOrder  = \(ws:l:t:exs) -> exs++[ws,l,t]
   }

-- Handy helpers
spawnOnWs ws command = (windows $ W.greedyView ws) >> spawn command
