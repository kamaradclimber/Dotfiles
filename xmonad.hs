{-# LANGUAGE NoMonomorphismRestriction #-}

-- {{{ Imports
import XMonad
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.NoBorders
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook hiding (DzenUrgencyHook)
import XMonad.Layout.Accordion
import XMonad.Layout.Decoration
import XMonad.Layout.Grid
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
--import XMonad.Prompt.RunOrRaise
--import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.Dzen (dzenWithArgs, seconds)
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.EZConfig(additionalKeys)

import qualified Data.Map as M
import Data.Monoid

import System.Exit
import System.IO
-- }}}

myTerminal           = "urxvt"
myBrowser            = "chromium"
myWorkspaces         = map show [1..8] ++ ["IM"]

myBorderWidth        = 1
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True


-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask = mod4Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--


-- {{{ Key bindings
myKeys = \c -> azertyKeys c `M.union` generalKeys c 


generalKeys conf@(XConfig {XMonad.modMask = modm }) = M.fromList $ [
    -- Spawn programs
    ((modm,                 xK_Return),     spawn $ XMonad.terminal conf),
    ((modm,                 xK_a),     scratchpadSpawnActionTerminal $ XMonad.terminal conf),
    ((modm,                 xK_c),          spawn myBrowser),
    ((modm,                 xK_F4),         kill),

    -- Layouts
    ((modm,                 xK_space),      sendMessage NextLayout),
    ((modm .|. shiftMask,   xK_space),      setLayout $ XMonad.layoutHook conf),
    ((modm,                 xK_n),          refresh),


    -- Focus
    ((modm,                 xK_Tab),        windows W.focusDown),
    ((modm,                 xK_Page_Down),  windows W.focusDown),
    ((modm,                 xK_Page_Up),    windows W.focusUp),
    --((modm,                 xK_m),          windows W.focusMaster),
    ((modm              ,   xK_u),          focusUrgent),

    -- Swap focused window
    --((modm,                 xK_Return),     windows W.swapMaster),
    --((modm .|. shiftMask,   xK_j     ),     windows W.swapDown  ),
    --((modm .|. shiftMask,   xK_k     ),     windows W.swapUp    ),
    ((modm .|. shiftMask,     xK_Return), dwmpromote),

    -- Resize
    ((modm,                 xK_Left),       sendMessage Shrink),
    ((modm,                 xK_Right),         sendMessage Expand),
    -- Push window back into tiling
    ((modm .|. shiftMask,   xK_t),          withFocused $ windows . W.sink),


    -- Toggle the status bar gap
    ((modm,                 xK_b),          sendMessage ToggleStruts),

    ((modm,                 xK_z),          withFocused toggleBorder ),
    ((modm,                 xK_F11),        withFocused (sendMessage . maximizeRestore)),


    ((modm .|. shiftMask,   xK_q),          io (exitWith ExitSuccess)),
    ((modm              ,   xK_F5),         spawn "xmonad --recompile; xmonad --restart")
    ] 
    -- }}}

-- {{{ Mouse bindings
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $ [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)),
    -- mod-button2, Raise the window to the top of the stack
    ((modm, button2), (\w -> focus w >> windows W.shiftMaster)),
    -- mod-button3, Set the window to floating mode and resize by dragging
    --, ((modm, button3), (\w -> focus w >> mouseResizeWindow w
    --                                   >> windows W.shiftMaster))
    ((modm, button3),               (\w -> focus w >> Sqr.mouseResizeWindow w False)),
    ((modm .|. shiftMask, button3), (\w -> focus w >> Sqr.mouseResizeWindow w True ))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
-- }}}

-- {{{ Layouts
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
defaultLayout  = avoidStruts . smartBorders . windowNavigation $ tiled ||| Mirror tiled ||| Grid |||     Full 
imLayout       = avoidStruts . smartBorders . windowNavigation $ kingTiled ||| Mirror tiled ||| Grid |||     Full 

myLayout = onWorkspace "IM" imLayout defaultLayout


  
kingTiled = Tall nmaster delta ratio
  where
    nmaster = 1        -- Windows in the master pane
    ratio   = 6/8      -- Proportion of screen occupied by master pane
    delta   = 3/100    -- Percent of screen to increment by when resizing panes
tiled    = Tall nmaster delta ratio
  where
    nmaster = 1        -- Windows in the master pane
    ratio   = 1/2      -- Proportion of screen occupied by master pane
    delta   = 3/100    -- Percent of screen to increment by when resizing panes
stack    = StackTile 1 (3/100) (1/2)
-- }}}

-- {{{ Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
floatingWindows = composeAll [
    --className =? "MPlayer"        --> doFloat,
    className =? "Gimp"           --> doFloat]
    
ignoredWindows = composeAll [
    resource  =? "desktop_window" --> doIgnore,
    resource  =? "kdesktop"       --> doIgnore ]

moveToWorkspace = composeAll [
    resource =?  "Pidgin"  --> doF (W.shift "IM")
     ]

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.4         -- terminal height
    w = 0.60        -- terminal width
    t = 1 - h       -- distance from top edge
    l = (1 - w)/2   -- distance from left edge

onNewWindow = 
    manageDocks <+>
    floatingWindows <+>
    ignoredWindows <+>
    moveToWorkspace <+>
    manageScratchPad
-- }}}

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook = docksEventHook

-- {{{ Status bars and logging
myLogHook pipe = do
    dynamicLogWithPP $ statusInfo pipe
    fadeInactiveLogHook 0.7

-- myINBOX = "/home/grego/Mail/Gmail/INBOX"
--tagL l = onLogger $ wrap (l++ ": ") ""
--myEmails = [tagL "U" $  maildirUnread myINBOX, tagL "N"$ maildirNew myINBOX ]

--myExtras = [date "%R", padL loadAvg] ++ myEmails 

statusInfo pipe = defaultPP {
    ppCurrent           = dzenColor "lightblue" "#0000aa",
    ppVisible           = wrap "(" ")",
    ppHidden            = (\i -> case i of
        "NSP" -> ""
        _     -> i),
    ppHiddenNoWindows   = \_ -> "",
    ppUrgent            = dzenColor "yellow" "red",
    ppSep               = " | ",
    ppWsSep             = " ",
    ppTitle             = dzenColor "#7777ff" "" . shorten 30,
    ppOrder             =  \(ws:_:_:rest) -> [ws]++rest,
    ppOutput            = hPutStrLn pipe
--  ,  ppExtras            = myExtras
    }
-- }}}

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
myStartupHook = return ()
data MyUrgencyHook = MyUrgencyHook {
                         dur :: Int, -- ^ number of microseconds to display the dzen
                                          --   (hence, you'll probably want to use 'seconds')
                         argss :: [String] -- ^ list of extra args (as 'String's) to pass to dzen
                       }
    deriving (Read, Show)
instance UrgencyHook MyUrgencyHook where
    urgencyHook MyUrgencyHook { dur = d, argss = a } w = do
        name <- getName w
        ws <- gets windowset
        whenJust (W.findTag w ws) (flash name)
      where flash name index =
                  dzenWithArgs (show name ++ " (go to " ++ index++")") a d
myDzenUrgencyHook = MyUrgencyHook {dur = seconds 5, argss = ["-bg", "cyan","-fg","red", "-xs", "1", "-x", "100", "-w", "300"] }

myUrgencyConfig = urgencyConfig { suppressWhen = Visible, remindWhen = Repeatedly 120 60}
myUrgencyHook = withUrgencyHookC myDzenUrgencyHook myUrgencyConfig
--myUrgencyHook = withUrgencyHook NoUrgencyHook 



-- {{{ Entry point
main = do
    spawn "~/.dotfiles/dzen.sh | dzen2 -x 500 -p  -ta r -expand \"r\""
    dzenPipe <- spawnPipe "dzen2 -ta \"l\" -w 480 "
    _ <- spawn myTerminal
    xmonad $ myUrgencyHook $ defaults dzenPipe

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
defaults pipe = defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = myLayout,
    manageHook         = onNewWindow,
    handleEventHook    = myEventHook,
    logHook            = myLogHook pipe,
    startupHook        = myStartupHook
    }
-- }}}
