-- xmonad config.hs


-- Imports
     -- Base 
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

     -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

     -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

     --Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

     -- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile 
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

     -- Layout modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

     -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce


myFont :: String
myFont = "xft:Fira Code:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "brave"

myEditor :: String
myEditor = myTerminal ++ " -e nvim"

-- ROFI
appLauncher = "rofi -show run"

-- follow mouse focus
myFoucsFollowMouse :: Bool
myFoucsFollowMouse = False

-- click a window to focus also passes click to window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of window border in pixels
myBorderWidth   = 2

-- border colors for unfocused and focused windows
myNormalBorderColor   = "#dddddd"
myFocusedBorderColor = "#ff0000"

-----------------------------------------------------------------------------

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-----------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
        spawnOnce "nitrogen --restore &"
        spawnOnce "picom &"
	spawnOnce "pa-applet &"
        spawnOnce "nm-applet &"
	spawnOnce "cbatticon &"
        spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x282c34 --height 22 &"

	setWMName "LG3D"

-----------------------------------------------------------------------------

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x28,0x2c,0x34) -- lowest inactive bg
		  (0x28,0x2c,0x34) -- highest inactive bg
		  (0xc7,0x92,0xea) -- active bg
		  (0xc0,0xa7,0x9a) -- inactve fg
		  (0x28,0x2c,0x34) -- active fg


-----------------------------------------------------------------------------

mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight = 40
    , gs_cellwidth = 200
    , gs_cellpadding = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font = myFont
    }

-----------------------------------------------------------------------------

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight = 40
		   , gs_cellwidth = 200
		   , gs_cellpadding = 6
		   , gs_originFractX = 0.5
		   , gs_originFractY = 0.5
		   , gs_font = myFont
		   }

-----------------------------------------------------------------------------

myAppGrid =[ ("", "")]

-----------------------------------------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm]
  where
    spawnTerm = myTerminal ++ " -t scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
	         h = 0.9
		 w = 0.9
		 t = 0.95 -h
		 l = 0.95 -w

-----------------------------------------------------------------------------

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-----------------------------------------------------------------------------

tall = renamed [Replace "tall"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 12
    $ mySpacing 8
    $ ResizableTall 1 (3/100) (1/2) []
mmagnify = renamed [Replace "magnify"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ magnifier 
    $ limitWindows 12
    $ mySpacing 8
    $ ResizableTall 1 (3/100) (1/2) []
monocle = renamed [Replace "monocle"]
    $ smartBorders 
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 20 Full
floats = renamed [Replace "floats"]
    $ smartBorders
    $ limitWindows 20 simplestFloat
grid = renamed [Replace "grid"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 12
    $ mySpacing 8
    $ mkToggle (single MIRROR)
    $ Grid (16/10)
spirals = renamed [Replace "spirals"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ mySpacing' 8
    $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 7
    $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 7
    $ Mirror
    $ ThreeCol 1 (3/100) (1/2)
tabs = renamed [Replace "tabs"]
    $ tabbed shrinkText myTabTheme
tallAccordion = renamed [Replace "tallAccordion"]
    $ Accordion
wideAccordion = renamed [Replace "wideAccordion"]
    $ Mirror Accordion

-----------------------------------------------------------------------------

myTabTheme = def { fontName = myFont
                 , activeColor = "#46d9ff"
		 , inactiveColor = "#313846"
		 , activeBorderColor = "#46d9ff"
		 , inactiveBorderColor = "#282c34"
		 , activeTextColor = "#282c34"
		 , inactiveTextColor = "#d0d0d0"
		 }

-----------------------------------------------------------------------------

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font = "xft:Ubuntu:bold:size=60"
    , swn_fade = 1.0
    , swn_bgcolor = "#1c1f24"
    , swn_color = "#ffffff"
    }

-----------------------------------------------------------------------------

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats 
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
	     where
	       myDefaultLayout = withBorder myBorderWidth tall
	                        ||| mmagnify
				||| noBorders monocle
				||| floats
				||| noBorders tabs
				||| grid
				||| spirals
				||| threeCol
				||| threeRow
				||| tallAccordion
				||| wideAccordion

-----------------------------------------------------------------------------

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] 

-----------------------------------------------------------------------------

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    -- 'doFloat' forces a window to float
    -- 'doShift ( myWorkspaces !! 7)' send program to workspace 8!!!
    [ className =? "confirm" --> doFloat
    , className =? "file_progress" --> doFloat
    , className =? "dialog" --> doFloat
    , className =? "download" --> doFloat
    , className =? "error" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "notification" --> doFloat
    , className =? "pinetry-gtk-2" --> doFloat
    , className =? "splash" --> doFloat
    , className =? "toolbar" --> doFloat
    , className =? "Yad" --> doCenterFloat
    , title =? "Oracle VM VirtualBox Manager" --> doFloat
    , className =? "Brave-browser" --> doShift ( myWorkspaces !! 1) 
    , className =? "mpv" --> doShift ( myWorkspaces !! 7) 
    , className =? "Gimp" --> doShift ( myWorkspaces !! 8) 
    , className =? "VirtualBox Manager" --> doShift ( myWorkspaces !! 4) 
    , isFullscreen --> doFullFloat
    ] <+> namedScratchpadManageHook myScratchPads

-----------------------------------------------------------------------------

--Key binds

myKeys :: [(String, X ())]
myKeys = 
    -- KB_GROUP Xmonad
        [ ("M-S-r", spawn "xmonad -- restart")  --Restarts xmonad
	, ("M-S-q", io exitSuccess) -- Quits xmonad

    -- KB_GROUP Run prompt
        , ("M-d", spawn appLauncher)

    -- KB_GROUP Launch useful programs
        , ("M-<return>", spawn (myTerminal))
	, ("M-b", spawn (myBrowser))

    -- KB_GROUP Kill windows
        , ("M-S-c", kill1)
	, ("M-S-a", killAll)

    -- KB_GROUP Workspaces
        , ("M-.", nextScreen)
	, ("M-,", prevScreen)
	, ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)
	, ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)

    -- KB_GROUP Floating windows
        , ("M-f", sendMessage (T.Toggle "floats"))
	, ("M-t", withFocused $ windows . W.sink)
	, ("M-S-t", sinkAll)

    -- KB_GROUP Increase/decrease spacing (gaps)
        , ("C-M1-j", decWindowSpacing 4)
	, ("C-M1-k", incWindowSpacing 4)
	, ("C-M1-h", decWindowSpacing 4)
	, ("C-M1-l", incWindowSpacing 4)
    
    -- KB_GROUP Grid Select (CTR-g followed by a key)
        , ("C-g g", spawnSelected' myAppGrid)
        , ("C-g t", goToSelected $ mygridConfig myColorizer)
        , ("C-g b", bringSelected $ mygridConfig myColorizer)

    -- KB_GROUP Windows navigation
        , ("M-m", windows W.focusMaster)
	, ("M-j", windows W.focusDown)
	, ("M-k", windows W.focusUp)
	, ("M-S-m", windows W.swapMaster)
	, ("M-S-j", windows W.swapDown)
	, ("M-S-k", windows W.swapUp)
	, ("M-<Backspace>", promote)
	, ("M-S-<Tab>", rotSlavesDown)
	, ("M-C-<Tab>", rotAllDown)

    -- KB_GROUP Layouts
        , ("M-<Tab>", sendMessage NextLayout)
	, ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)

    -- KB_GROUP Increase/Decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))
	, ("M-S-<Down>", sendMessage (IncMasterN (-1)))
	, ("M-C-<Up>", increaseLimit)
	, ("M-C-<Down>", decreaseLimit)

    -- KB_GROUP Window resizing 
        , ("M-h", sendMessage Shrink)
	, ("M-l", sendMessage Expand)
	, ("M-M1-j", sendMessage MirrorShrink)
	, ("M-M1-k", sendMessage MirrorExpand)

    -- KB_GROUP Sublayouts (put all into sublayout or pull them out)
        , ("M-C-h", sendMessage $ pullGroup L)
	, ("M-C-l", sendMessage $ pullGroup R)
	, ("M-C-k", sendMessage $ pullGroup U)
	, ("M-C-j", sendMessage $ pullGroup D)
	, ("M-C-m", withFocused (sendMessage . MergeAll))
	, ("M-C-/", withFocused (sendMessage . UnMergeAll))
	, ("M-C-.", onGroup W.focusUp')
	, ("M-C-,", onGroup W.focusDown')

    -- KB_GROUP Scratchpads
        , ("M-s t", namedScratchpadAction myScratchPads "terminal")
        ]
	  where nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))
	        nonEmptyNonNSP = WSIs(return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

-----------------------------------------------------------------------------

-- main
main = do
    xmproc <- spawnPipe "xmobar -x 0"

    xmonad $ ewmh def
  	{ manageHook = myManageHook <+> manageDocks
	, handleEventHook = docksEventHook

	, modMask = myModMask
	, terminal = myTerminal

	, startupHook = myStartupHook
	, layoutHook = showWName' myShowWNameTheme $ myLayoutHook

	, workspaces = myWorkspaces
	, borderWidth = myBorderWidth
	, normalBorderColor = myNormalBorderColor
	, focusedBorderColor = myFocusedBorderColor

	, logHook = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
	    -- pp variable for xmobar
	    { ppOutput = \x -> hPutStrLn xmproc x

	    -- Current Workspace
	    , ppCurrent = xmobarColor "#c792ea" "" . wrap "<box type=Bottom width=2 mb=2 color=#c792ea>" "</box>"

	    -- Visible but not current workspace
	    , ppVisible = xmobarColor "#c792ea" ""

	    -- Hidden workspace 
	    , ppHidden = xmobarColor "#82AAFF" "" . wrap "<box type=Top width=2 mt=2 color=#82AAFF>" "</box>" 

	    -- Hidden workspace (no windows)
	    , ppHiddenNoWindows = xmobarColor "#82AAFF" ""

	    -- Title of active window
	    , ppTitle = xmobarColor "#b3afc2" "" . shorten 60

	    -- Separator character
	    , ppSep = "<fc=#666666> <fn=1>|</fn> </fc>"

	    -- Urgent workspace
	    , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"

	    -- # of windows current workspace
	    , ppExtras = [windowCount]
	    
	    -- order of things in xmobar
	    , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
	    }
	} `additionalKeysP` myKeys
