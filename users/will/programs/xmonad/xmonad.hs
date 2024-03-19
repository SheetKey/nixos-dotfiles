import XMonad

import qualified XMonad.StackSet as W

-- This is in version 0.18.0
-- Nix currently has 0.17.1
-- import XMonad.Actions.ToggleFullFloat (toggleFullFloatEwmhFullscreen, toggleFullFloat)

import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)

import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Ungrab (unGrab)
import XMonad.Prompt.Shell (split)

-- layouts


-- layouts modifiers
import XMonad.Layout.MultiToggle (Toggle(..),  mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR))

-- status bar (using dzen2)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks (docks, avoidStruts, ToggleStruts(ToggleStruts))
import XMonad.Util.Loggers

-- containers
import qualified Data.Map as M

-- process
import System.Process (shell, readCreateProcess)

-- time
import Data.Time (formatTime
                 , defaultTimeLocale
                 , addUTCTime
                 , secondsToNominalDiffTime
                 , getCurrentTime)

-- base
import System.Exit (exitSuccess)

main :: IO ()
main = do
  xmonad $
    --toggleFullFloatEwmhFullscreen $
    ewmhFullscreen $ ewmh $
    docks $ dynamicSBs myStatusBars $
    def
    { modMask = mod4Mask
    , keys = myKeys
    , layoutHook = myLayout
    , terminal = "alacritty"
    , startupHook = myStartupHook
    , focusFollowsMouse = False
    , clickJustFocuses = True
    , focusedBorderColor = "#ffffff"
    , normalBorderColor = "#000000"
    }

-- NOTE:
-- The following are leader keys and should not be used on their own:
-- "M-l" for layout related commands
-- "M-u" for other commands
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys c = mkKeymap c $
  [ ("M-<Return>", spawn $ terminal c)
  , ("M-S-c", kill)
  , ("M-x", spawn "rofi -show run")
  , ("M-S-b", spawn "brave")
  , ("M-S-s", unGrab *> spawn "spectacle -r")
  , ("M-e", spawn "emacsclient -c")
    
  -- Layout keys
  , ("M-<Tab>", sendMessage NextLayout)
  , ("M-n", windows W.focusDown)
  , ("M-p", windows W.focusUp)
  , ("M-m", windows W.focusMaster)
  , ("M-S-<Return>", windows W.swapMaster)
  , ("M-S-n", windows W.swapDown)
  , ("M-S-p", windows W.swapUp)
  -- resize
  , ("M-f", sendMessage Expand)
  , ("M-b", sendMessage Shrink)
  -- version issues
  -- , ("M-<Space>", withFocused toggleFullFloat)
  , ("M-<Space>", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)
  , ("M-l m", sendMessage (Toggle MIRROR))
  
  , ("M-S-q", io exitSuccess)

  -- other
  , ("<XF86MonBrightnessUp>", spawn "brightnessctl s +10%")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 10%-")
  , ("M-u b h", spawn "brightnessctl s 100%")
  , ("M-u b l", spawn "brightnessctl s 15%")
  ]
  ++
  -- mod-[1-9] switches to workspace N
  [ ("M-" ++ show k, windows $ W.greedyView i)
  | (i, k) <- zip (XMonad.workspaces c) [1..9]]
  ++
  -- mod-shift-[1-9] moves client to workspace N
  [ ("M-S-" ++ show k, windows $ W.shift i)
  | (i, k) <- zip (XMonad.workspaces c) [1..9]]
  
myLayout = transformLayout $ defTiled
  where
    defTiled = Tall 1 (3/100) (1/2)

transformLayout = id
  . avoidStruts
  . mkToggle (single NBFULL)
  . mkToggle (single MIRROR)

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "trayer \
            \--edge top \
            \--align right \
            \--widthtype request \
            \--SetDockType true \
            \--SetPartialStrut true \
            \--expand true \
            \--transparent true \
            \--alpha 0 \
            \--tint 0x535353 \
            \--height 22 \
            \--iconspacing 4 \
            \&"
  spawnOnce "feh --bg-fill --no-fehbg \
            \~/dotfiles/nixos-dotfiles/wallpaper/green-aesthetic-wallpaper-20.jpg"
  spawnOnce "picom &"
  spawnOnce "pa-applet &"
  spawnOnce "nm-applet &"
  spawnOnce "cbatticon &"
  spawnOnce "dzen2-nix-icon&"
  spawnOnce "emacs --daemon &"
  -- spawnOnce "xfce4-power-manager &"
  
-- dzen status bar
myStatusBars :: ScreenId -> IO StatusBarConfig
myStatusBars _ = do
  -- trayerWidth <- readCreateProcess
  --   (shell "xprop -name panel | grep 'program specified minimum size' | cut -d ' ' -f 5") ""
  lBar <- statusBarPipe dzenCmdLeft (pure dzenLeftPP)
  rBar <- statusBarPipe dzenCmdRight (pure dzenRightPP)
  return $ lBar <> rBar

dzenCmdLeft :: String
dzenCmdLeft = "dzen2 \
              \-dock \
              \-ta l \
              \-bg '#000000' \
              \-fg '#ffffff' \
              \-fn '-*-firacode nerd font mono-bold-*-*-*-*-90-*-*-*-*-iso8859-1' \
              \-h 22 \
              \-w 1170 \
              \-x 30 \
              \-e 'button2=;'"

dzenCmdRight :: String
dzenCmdRight = "dzen2 \
               \-dock \
               \-ta r \
               \-bg '#000000' \
               \-fg '#ffffff' \
               \-fn '-*-firacode nerd font mono-bold-*-*-*-*-90-*-*-*-*-iso8859-1' \
               \-h 22 \
               \-w 662 \
               \-x 1200 \
               \-e 'button2=;'"

filterWindowTitle :: String -> String
filterWindowTitle = last . split '-'

dzenLeftPP :: PP
dzenLeftPP = def
  { ppCurrent = dzenColor "#ffffff" "#535353" . pad
  , ppVisible = dzenColor "#ffffff" "#000000" . pad
  , ppHidden = dzenColor "#ffffff" "#000000" . pad
  , ppHiddenNoWindows = const ""
  , ppUrgent = dzenColor "#ffffff" "#ff5f59" . pad
  , ppWsSep = ""
  , ppSep = ""
  , ppTitle = const ""
  , ppTitleSanitize = const ""
  , ppLayout = pad
  , ppOrder = \ (ws : l : _ : ts : _) -> [l, ws, ts]
  , ppExtras =
    [ logTitles' $ TitlesFormat
      { focusedFormat = dzenColor "#ffffff" "#535353" 
                        . pad . dzenEscape . filterWindowTitle
      , unfocusedFormat = dzenColor "#ffffff" "#000000" 
                          . pad . dzenEscape . filterWindowTitle
      , urgentFormat = dzenColor "#ffffff" "#ff5f59" 
                       . pad . dzenEscape . wrap "!" "!" . drop 2 . dropWhile (/= '-')
      }
    ]
  }

dzenRightPP :: PP
dzenRightPP = def
  { ppOrder = \ (_ : _ : _ : d : _) -> [d]
  , ppExtras =
    [ dzenColorL "#ffffff" "#000000" $ padL myDateLogger
    ]
  }

myDateLogger :: Logger
myDateLogger = io $ Just
  . formatTime defaultTimeLocale "%a %b %d, %I:%M%P"
  . addUTCTime (secondsToNominalDiffTime (-14400))
  <$> getCurrentTime
