import XMonad

import qualified XMonad.StackSet as W

import XMonad.Actions.ToggleFullFloat (toggleFullFloatEwmhFullscreen, toggleFullFloat)

import XMonad.Hooks.EwmhDesktops

import XMonad.Util.EZConfig (mkKeymap)


main :: IO ()
main = do
  xmonad $ toggleFullFloatEwmhFullscreen $ ewmhFullscreen $ ewmh $ def
    { modMask = mod4Mask
    , keys = myKeys
    }
    

myKeys :: XConfig Layout -> Map (ButtonMask, Button) (Window -> X ())
myKeys c = mkKeymap c $
  [ ("M-S-<Return>", spawn $ terminal c)
  , ("M-S-c", kill)
  , ("M-x M-c", kill)
  , ("M-f", spawn "rofi -show run")
  , ("M-b", spawn "brave")
  , ("M-S-s", unGrab *> spawn "spectacle -r"
    
  -- Layout keys
  , ("M-<Tab>", sendMessage NextLayout)
  , ("M-n", windows W.focusDown)
  , ("M-p", windows W.focusUp)
  , ("M-m", windows W.focusMaster)
  , ("M-<Return>", window W.swapMaster)
  , ("M-S-n", windows W.swapDown)
  , ("M-S-p", windows W.swapUp)
  , ("M-S-f", sendMessage Shrink)
  , ("M-S-b", sendMessage Expand)
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-<Space>", withFocused toggleFullFloat)
  
  , ("M-S-q", io exitSuccess)
  ]
  ++
   [((m .|. modMask c, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  
