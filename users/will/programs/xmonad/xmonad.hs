import XMonad

import qualified XMonad.StackSet as W

-- This is in version 0.18.0
-- Nix currently has 0.17.1
-- import XMonad.Actions.ToggleFullFloat (toggleFullFloatEwmhFullscreen, toggleFullFloat)

import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)

import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Ungrab (unGrab)

-- layouts modifiers
import XMonad.Layout.MultiToggle (Toggle(..),  mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR))
import XMonad.Layout.NoBorders (smartBorders)

-- status bar (using dzen2)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks (docks, avoidStruts, ToggleStruts(ToggleStruts))

-- containers
import qualified Data.Map as M

-- process
import System.Process (shell, readCreateProcess)

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
    , layoutHook = avoidStruts myLayout
    , terminal = "alacritty"
    , startupHook = myStartupHook
    , focusFollowsMouse = False
    , clickJustFocuses = False
    }
    
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys c = mkKeymap c $
  [ ("M-<Return>", spawn $ terminal c)
  , ("M-S-c", kill)
  , ("M-x M-c", kill)
  , ("M-f", spawn "rofi -show run")
  , ("M-b", spawn "brave")
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
  , ("M-S-f", sendMessage Shrink)
  , ("M-S-b", sendMessage Expand)
  , ("M-t", withFocused $ windows . W.sink)
  -- version issues
  -- , ("M-<Space>", withFocused toggleFullFloat)
  , ("M-<Space>", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)
  , ("M-x m", sendMessage (Toggle MIRROR))
  
  , ("M-S-q", io exitSuccess)
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
  . smartBorders
  . mkToggle (single NBFULL)
  . mkToggle (single MIRROR)

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "trayer --edge top --align right --SetDockType true \
            \--SetPartialStrut true --expand true --width 10 \
            \--transparent true --tint 0x5f5f5f --height 20"
  spawnOnce "feh --bg-fill --no-fehbg \
            \~/dotfiles/nixos-dotfiles/wallpaper/green-aesthetic-wallpaper-20.jpg"
  spawnOnce "picom &"
  spawnOnce "pa-applet &"
  spawnOnce "nm-applet &"
  spawnOnce "cbatticon &"
  spawnOnce "emacs --daemon &"
  -- spawnOnce "xfce4-power-manager &"

-- dzen status bar
myStatusBars :: ScreenId -> IO StatusBarConfig
myStatusBars _ = do
  -- trayerWidth <- readCreateProcess
  --   (shell "xprop -name panel | grep 'program specified minimum size' | cut -d ' ' -f 5") ""
  statusBarPipe (dzenCmd "1728") (pure dzenPP)

dzenCmd :: String -> String
dzenCmd width = "dzen2 -dock -ta l -fn \"-misc-firacode nerd font mono-medium-r-normal--0-12-0-0-m-0-iso10646-1\" -w " ++ width
