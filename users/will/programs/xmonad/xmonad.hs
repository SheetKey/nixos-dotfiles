import XMonad

import qualified XMonad.StackSet as W

import XMonad.Actions.ToggleFullFloat (toggleFullFloatEwmhFullscreen, toggleFullFloat)

import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)

import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Ungrab (unGrab)
import XMonad.Prompt.Shell (split)

-- actions
import XMonad.Actions.Navigation2D
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.WithAll (withAll)

-- layouts
import XMonad.Layout.BinarySpacePartition

-- layouts modifiers
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutModifier
import XMonad.Util.NamedWindows
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Layout.NoBorders (lessBorders, Ambiguity(OnlyScreenFloat))

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
import Control.Monad (guard)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import System.Exit (exitSuccess)
import GHC.IO.Handle (hDuplicateTo)
import System.IO (stderr, stdout, openFile, hClose, hSetBuffering, BufferMode(..), IOMode(..))
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)

main :: IO ()
main = do
  -- redirect stderr and stdout
  home <- getEnv "HOME"
  let xmonadDir = home </> ".xmonad"
  createDirectoryIfMissing False xmonadDir
  hClose stdout
  hClose stderr
  stdout' <- openFile (xmonadDir </> "xmonad-stdout.log") AppendMode
  stderr' <- openFile (xmonadDir </> "xmonad-stderr.log") AppendMode
  hDuplicateTo stdout' stdout
  hDuplicateTo stderr' stderr
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  -- config
  xmonad $
    toggleFullFloatEwmhFullscreen $
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
  , ("M-e", spawn "emacsclient -c --eval '(org-agenda-list)' '(delete-other-windows)'")
    
  -- Layout keys
  , ("M-<Tab>", sendMessage NextLayout)
  , ("M-m", windows W.focusMaster)
  , ("M-S-<Return>", windows W.swapMaster)
  -- toggle float
  , ("M-l f", withFocused toggleFloat)
  -- BSP layout keys
  , ("M-M1-b", sendMessage $ ExpandTowards L)
  , ("M-M1-f", sendMessage $ ExpandTowards R)
  , ("M-M1-n", sendMessage $ ExpandTowards D)
  , ("M-M1-p", sendMessage $ ExpandTowards U)
  , ("M-M1-C-b", sendMessage $ ShrinkFrom L)
  , ("M-M1-C_f", sendMessage $ ShrinkFrom R)
  , ("M-M1-C-n", sendMessage $ ShrinkFrom D)
  , ("M-M1-C-p", sendMessage $ ShrinkFrom U)
  , ("M-M1-s", sendMessage Swap)
  , ("M-M1-r", sendMessage Rotate)
  -- Fullscreen
  , ("M-<Space>", withFocused toggleFullFloat)
  -- hide windows
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

toggleFloat :: Window -> X ()
toggleFloat w = do
  (sc, rr) <- floatLocation w
  windows $ \ws -> toggleFloat_ w rr . fromMaybe ws $ do
    i <- W.findTag w ws
    guard $ i `elem` map (W.tag . W.workspace) (W.screens ws)
    f <- W.peek ws
    sw <- W.lookupWorkspace sc ws
    return (W.focusWindow f . W.shiftWin sw w $ ws)
  

toggleFloat_ :: Ord a => a -> W.RationalRect -> W.StackSet i l a s sd -> W.StackSet i l a s sd
toggleFloat_ w r s =
  s { W.floating = M.alter
                 (\ mFloat -> case mFloat of
                                Just _ -> Nothing
                                Nothing -> Just r
                 )
                 w (W.floating s)
    }
  
myLayout = transformLayout $ emptyBSP ||| tallLeft
  where
    tallLeft = Tall 1 (3/100) (1/2)

transformLayout = id
  . avoidStruts
  . hiddenWindows
  . lessBorders OnlyScreenFloat

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
filterWindowTitle [] = []
filterWindowTitle str  = dropWhile (== ' ') . last . split '-' $ str

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
  , ppOrder = \ (ws : l : _ : ts : hts : _) -> [l, ws, " ", ts, " ", hts]
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
               | GetHiddenWindowNames             -- ^ Get the names of hidden windows.
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
    | Just GetHiddenWindowNames          <- fromMessage mess = getHiddenNames h
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
  -- XS.modifyM $ \ (HiddenWindowTitles ts) -> do
  --   name <- fmap show . getName $ win
  --   return $ HiddenWindowTitles $ name : ts
  return . Just . HiddenWindows $ hidden ++ [win]

--------------------------------------------------------------------------------
popNewestMsg :: HiddenWindows a -> X (Maybe (HiddenWindows a))
popNewestMsg (HiddenWindows [])     = return Nothing
popNewestMsg (HiddenWindows hidden) = do
  let (win, rest) = (last hidden, init hidden)
  restoreWindow win
  -- XS.modify $ \ (HiddenWindowTitles (_ : ts)) -> HiddenWindowTitles ts
  return . Just . HiddenWindows $ rest

--------------------------------------------------------------------------------
popOldestMsg :: HiddenWindows a -> X (Maybe (HiddenWindows a))
popOldestMsg (HiddenWindows [])         = return Nothing
popOldestMsg (HiddenWindows (win:rest)) = do
  restoreWindow win
  -- XS.modify $ \ (HiddenWindowTitles ts) -> HiddenWindowTitles (init ts)
  return . Just . HiddenWindows $ rest

--------------------------------------------------------------------------------
popSpecificMsg :: Window -> HiddenWindows a -> X (Maybe (HiddenWindows a))
popSpecificMsg _   (HiddenWindows []) = return Nothing
popSpecificMsg win (HiddenWindows hiddenWins) = if win `elem` hiddenWins
  then do
    restoreWindow win
    -- XS.modifyM $ \ (HiddenWindowTitles ts) -> do
    --   name <- fmap show . getName $ win
    --   return $ HiddenWindowTitles $ filter (/= name) ts
    return . Just . HiddenWindows $ filter (/= win) hiddenWins
  else
    return . Just . HiddenWindows $ hiddenWins

--------------------------------------------------------------------------------
-- | Add the names of hidden workspaces to 'extendedStack' so they may be
-- printed to the status bar.
getHiddenNames :: HiddenWindows a -> X (Maybe (HiddenWindows a))
getHiddenNames (HiddenWindows hidden) = do
  names <- traverse (fmap show . getName) hidden
  XS.put $ HiddenWindowTitles names
  return Nothing

--------------------------------------------------------------------------------
restoreWindow :: Window -> X ()
restoreWindow win = modify (\s -> s { windowset = W.insertUp win $ windowset s })
-- restoreWindow w = windows $ \ss -> if w `W.member` ss
--                                    then w `W.insertUp` ss
--                                    else ss

--------------------------------------------------------------------------------
hiddenWinTitlesL :: Logger
hiddenWinTitlesL = do
  sendMessage GetHiddenWindowNames
  HiddenWindowTitles winTitles <- XS.get
  if winTitles == []
    then return $ Just ""
    else return . Just . unwords $ formatHidden <$> winTitles

formatHidden :: String -> String
formatHidden = dzenColor "#646464" "#303030" . pad . dzenEscape . filterWindowTitle

data HiddenWindowTitles = HiddenWindowTitles [String]
instance ExtensionClass HiddenWindowTitles where
  initialValue = HiddenWindowTitles []
