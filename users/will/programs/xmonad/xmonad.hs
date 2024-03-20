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

import XMonad.Actions.Navigation2D

-- layouts


-- layouts modifiers
import XMonad.Layout.MultiToggle (Toggle(..),  mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR))
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutModifier
import XMonad.Util.NamedWindows
import qualified XMonad.Util.ExtensibleState as XS

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
import Data.List ((\\))

main :: IO ()
main = do
  xmonad $
    --toggleFullFloatEwmhFullscreen $
    ewmhFullscreen $ ewmh $
    docks $ dynamicSBs myStatusBars $
    myNavigation2D $
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

myNavigation2D :: XConfig l -> XConfig l
myNavigation2D = navigation2DP
  ( def { defaultTiledNavigation = centerNavigation } )
  ("p", "b", "n", "f")
  [("M-", windowGo), ("M-S-", windowSwap)]
  True

-- NOTE:
-- The following are leader keys and should not be used on their own:
-- "M-l" for layout related commands
-- "M-u" for other commands
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys c = mkKeymap c $
  [ ("M-<Return>", spawn $ terminal c)
  , ("M-S-c", kill)
  , ("M-x", spawn "rofi -show run")
  , ("M-C-b", spawn "brave")
  , ("M-S-s", unGrab *> spawn "spectacle -r")
  , ("M-e", spawn "emacsclient -c")
    
  -- Layout keys
  , ("M-<Tab>", sendMessage NextLayout)
  , ("M-m", windows W.focusMaster)
  , ("M-S-<Return>", windows W.swapMaster)
  -- resize
  , ("M-l f", sendMessage Expand)
  , ("M-l b", sendMessage Shrink)
  -- version issues
  -- , ("M-<Space>", withFocused toggleFullFloat)
  , ("M-<Space>", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)
  , ("M-l m", sendMessage (Toggle MIRROR))
  , ("M-h", withFocused hideWindow)
  , ("M-C-h", popNewestHiddenWindow)
  
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
  
myLayout = transformLayout $ tallLeft
  where
    tallLeft = Tall 1 (3/100) (1/2)

transformLayout = id
  . avoidStruts
  . hiddenWindows
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
filterWindowTitle = dropWhile (== ' ') . last . split '-'

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
  , ppLayout = pad . (\\ "Hidden ")
  , ppOrder = \ (ws : l : _ : ts : hts : _) -> [l, ws, ts, hts]
  , ppExtras =
    [ logTitles' $ TitlesFormat
      { focusedFormat = dzenColor "#ffffff" "#535353" 
                        . pad . dzenEscape . filterWindowTitle
      , unfocusedFormat = dzenColor "#ffffff" "#000000" 
                          . pad . dzenEscape . filterWindowTitle
      , urgentFormat = dzenColor "#ffffff" "#ff5f59" 
                       . pad . dzenEscape . wrap "!" "!" . drop 2 . dropWhile (/= '-')
      }
    , hiddenWinTitlesL
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

-- THE FOLLOWING IS PROVIDED BY Xmonad.Layout.Hidden
-- However, this module does not expose the 'HiddenWindows' constructor,
-- so, as far as I can tell, it is not possible to get the names
-- of hidden windows to display on the bar
--------------------------------------------------------------------------------
newtype HiddenWindows a = HiddenWindows [Window] deriving (Show, Read)

--------------------------------------------------------------------------------
-- | Messages for the @HiddenWindows@ layout modifier.
data HiddenMsg = HideWindow Window                -- ^ Hide a window.
               | PopNewestHiddenWindow            -- ^ Restore window (FILO).
               | PopOldestHiddenWindow            -- ^ Restore window (FIFO).
               | PopSpecificHiddenWindow Window   -- ^ Restore specific window.
               deriving (Eq)

instance Message HiddenMsg

--------------------------------------------------------------------------------
instance LayoutModifier HiddenWindows Window where
  handleMess h@(HiddenWindows hidden) mess
    | Just (HideWindow win)              <- fromMessage mess = hideWindowMsg h win
    | Just PopNewestHiddenWindow         <- fromMessage mess = popNewestMsg h
    | Just PopOldestHiddenWindow         <- fromMessage mess = popOldestMsg h
    | Just (PopSpecificHiddenWindow win) <- fromMessage mess = popSpecificMsg win h
    | Just ReleaseResources              <- fromMessage mess = doUnhook
    | otherwise                                              = return Nothing
    where doUnhook = do mapM_ restoreWindow hidden
                        return Nothing

  modifierDescription _ = "Hidden"

--------------------------------------------------------------------------------
-- | Apply the @HiddenWindows@ layout modifier.
hiddenWindows :: LayoutClass l Window => l Window -> ModifiedLayout HiddenWindows l Window
hiddenWindows = ModifiedLayout $ HiddenWindows []

--------------------------------------------------------------------------------
-- | Remove the given window from the current layout.  It is placed in
-- list of hidden windows so it can be restored later.
hideWindow :: Window -> X ()
hideWindow = sendMessage . HideWindow

--------------------------------------------------------------------------------
-- | Restore a previously hidden window.  Using this function will
-- treat the list of hidden windows as a FIFO queue.  That is, the
-- first window hidden will be restored.
popOldestHiddenWindow :: X ()
popOldestHiddenWindow = sendMessage PopOldestHiddenWindow

--------------------------------------------------------------------------------
-- | Restore a previously hidden window.  Using this function will
-- treat the list of hidden windows as a FILO queue.  That is, the
-- most recently hidden window will be restored.
popNewestHiddenWindow :: X ()
popNewestHiddenWindow = sendMessage PopNewestHiddenWindow

popHiddenWindow :: Window -> X ()
popHiddenWindow = sendMessage . PopSpecificHiddenWindow

--------------------------------------------------------------------------------
hideWindowMsg :: HiddenWindows a -> Window -> X (Maybe (HiddenWindows a))
hideWindowMsg (HiddenWindows hidden) win = do
  modify (\s -> s { windowset = W.delete' win $ windowset s })
  XS.modifyM $ \ (HiddenWindowTitles ts) -> do
    name <- fmap show . getName $ win
    return $ HiddenWindowTitles $ name : ts
  return . Just . HiddenWindows $ hidden ++ [win]

--------------------------------------------------------------------------------
popNewestMsg :: HiddenWindows a -> X (Maybe (HiddenWindows a))
popNewestMsg (HiddenWindows [])     = return Nothing
popNewestMsg (HiddenWindows hidden) = do
  let (win, rest) = (last hidden, init hidden)
  restoreWindow win
  XS.modify $ \ (HiddenWindowTitles (_ : ts)) -> HiddenWindowTitles ts
  return . Just . HiddenWindows $ rest

--------------------------------------------------------------------------------
popOldestMsg :: HiddenWindows a -> X (Maybe (HiddenWindows a))
popOldestMsg (HiddenWindows [])         = return Nothing
popOldestMsg (HiddenWindows (win:rest)) = do
  restoreWindow win
  XS.modify $ \ (HiddenWindowTitles ts) -> HiddenWindowTitles (init ts)
  return . Just . HiddenWindows $ rest

--------------------------------------------------------------------------------
popSpecificMsg :: Window -> HiddenWindows a -> X (Maybe (HiddenWindows a))
popSpecificMsg _   (HiddenWindows []) = return Nothing
popSpecificMsg win (HiddenWindows hiddenWins) = if win `elem` hiddenWins
  then do
    restoreWindow win
    XS.modifyM $ \ (HiddenWindowTitles ts) -> do
      name <- fmap show . getName $ win
      return $ HiddenWindowTitles $ filter (/= name) ts
    return . Just . HiddenWindows $ filter (/= win) hiddenWins
  else
    return . Just . HiddenWindows $ hiddenWins

--------------------------------------------------------------------------------
restoreWindow :: Window -> X ()
restoreWindow = windows . W.insertUp

--------------------------------------------------------------------------------
hiddenWinTitlesL :: Logger
hiddenWinTitlesL = do
  HiddenWindowTitles winTitles <- XS.get
  return . Just . unwords $ formatHidden <$> winTitles

formatHidden :: String -> String
formatHidden = dzenColor "#646464" "#303030" . pad . dzenEscape . filterWindowTitle

data HiddenWindowTitles = HiddenWindowTitles [String]
instance ExtensionClass HiddenWindowTitles where
  initialValue = HiddenWindowTitles []
