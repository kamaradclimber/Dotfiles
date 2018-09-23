{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.NoBorders
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook hiding (DzenUrgencyHook)
import XMonad.Hooks.SetWMName
import XMonad.Layout.Accordion
import XMonad.Layout.Decoration
import XMonad.Layout.Grid
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Util.Dzen (dzenWithArgs, seconds)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Themes
import qualified XMonad.StackSet as W

import qualified Data.Map as M
import Data.Monoid

import System.Exit
import System.IO

myTerminal           = "urxvt"
myBrowser            = "firefox"
mySecondaryBrowser   = "firefox"
myWorkspaces         = map show [1..6] ++ ["CAL", "MAIL", "IM"] ++ map show [0]

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

-- all key config unions
-- NOT A GOOD IDEA TO ACTIVATE azertyKeys since it steals xK_z, xK_e, xK_r shortcut (for screens related shortcuts)
--myKeys c = bepoKeys c `M.union` azertyKeys c `M.union` generalKeys c
myKeys c = bepoKeys c `M.union` generalKeys c


workspace conf modm keys = M.fromList [
  ((m .|. modm, k), windows $ f i) | (i, k) <- zip (workspaces conf) keys,
   (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-- setup keys for bepo and qwerty
bepoKeys conf@(XConfig {modMask = modm}) = workspace conf modm [0x22,0xab,0xbb,0x28,0x29,0x40,0x2b,0x2d,0x2f,0x2a]
qwertyKeys conf@(XConfig {modMask = modm}) = workspace conf modm [0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x40]
-- for reference azertyKeys
-- [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0],

--Don't forget to describe each command
-- INFO: https://superuser.com/questions/389737/how-do-you-make-volume-keys-and-mute-key-work-in-xmonad to find new key code
generalKeys conf@(XConfig {XMonad.modMask = modm }) = M.fromList [
    -- Spawn programs
    ((modm,               xK_F1),         spawn "~/.dotfiles/keybinding_recall.sh"),             --the famous help
    ((modm,               xK_Return),     spawn $ XMonad.terminal conf),                         --new terminal
    ((modm,               xK_a),          scratchpadSpawnActionTerminal $ XMonad.terminal conf), --scratchpad
    ((modm, xK_backslash), withFocused (sendMessage . maximizeRestore)),
    ((modm,               xK_c),          spawn myBrowser),                                      --browser
    ((modm .|. shiftMask, xK_c),          spawn mySecondaryBrowser),                             -- secondary browser
    ((modm, xK_o),          spawn "dmenu_run"),                             -- any command
    ((modm,               xK_l),          spawn "~/img/lock.sh"),                                --lock screen
    ((modm,               xK_v),          spawn "/usr/bin/vim-anywhere vim urxvt"),                        -- launch vim-anywhere
    ((modm,               xK_F4),         kill),                                                 --kill current window
    -- Layouts
    ((modm,               xK_space),      sendMessage NextLayout),                               --next Layout
    ((modm .|. shiftMask, xK_space),      setLayout $ XMonad.layoutHook conf),                   --reset layout
    ((modm,               xK_e),          viewScreen XMonad.Actions.PhysicalScreens.verticalScreenOrderer 0),
    ((modm,               xK_t),          viewScreen XMonad.Actions.PhysicalScreens.verticalScreenOrderer 1),
    -- Focus
    ((modm,               xK_Tab),        windows W.focusDown),                                  --the famous alt-tab equivalent
    ((modm,               xK_u),          focusUrgent),                                          --go to the (last?) urgent windows
    -- Swap focused window
    ((modm .|. shiftMask, xK_Return),     dwmpromote),                                           --move windows to master area
    ((modm ,              xK_n),          moveTo Next EmptyWS),                                  --find next empty fallback
    ((modm .|. shiftMask, xK_n),          moveTo Next AnyWS),                                       --find next empty fallback
    -- Resize
    ((modm,               xK_Left),       sendMessage Shrink),                                   --shrink master area
    ((modm,               xK_Right),      sendMessage Expand),                                   --expand master area
    ((modm .|. shiftMask, xK_t),          withFocused $ windows . W.sink),                       --push window back into tiling

    ((modm,               xK_b),          sendMessage ToggleStruts),                             --toggle status bar gap
    ((modm,               xK_z),          withFocused toggleBorder ),                            --toggle window border

    ((0,                  0x1008ff11),    spawn "amixer set Master 2-"),
    ((0,                  0x1008ff13),    spawn "amixer set Master 2+"),


    -- xmonad lifecycle
    ((modm .|. shiftMask, xK_q),          io exitSuccess),                                       --leave xmonad
    ((modm,               xK_F5),         spawn "xmonad --recompile; xmonad --restart")          --apply modif
    ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster),
    -- mod-button2, Raise the window to the top of the stack
    ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    --, ((modm, button3), (\w -> focus w >> mouseResizeWindow w
    --                                   >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

----------------------------------------------------------------------
-- Layouts
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
threeColumns = ThreeCol 1 (3/100) (1/2)
threeLines   = Mirror (ThreeCol 1 (3/100) (1/3))


defaultLayout  = avoidStruts . smartBorders . windowNavigation $ tiled ||| Mirror tiled ||| Grid |||     Full ||| myTabLayout  ||| threeColumns ||| threeLines


myLayout = maximize $ defaultLayout

myTabConfig = theme kavonForestTheme
myTabLayout = tabbed shrinkText myTabConfig

tiled    = Tall nmaster delta ratio
  where
    nmaster = 1        -- Windows in the master pane
    ratio   = 1/2      -- Proportion of screen occupied by master pane
    delta   = 3/100    -- Percent of screen to increment by when resizing panes
stack    = StackTile 1 (3/100) (1/2)

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
    --className =? "Gimp"           --> doFloat
    className =? "Steam" --> doFloat
    ]
nonFloatingWindows = composeAll [
    className =? "rdesktop"         --> unfloat
    ]
    where unfloat = ask >>= doF . W.sink

ignoredWindows = composeAll [
--    resource  =? "desktop_window" --> doIgnore
    ]

noFocusWindows = composeAll [
     --role =? "browser"   --> doF W.focusDown --used to avoid focus on browser when opening new tab, fail!
    ]
    where role = stringProperty "WM_WINDOW_ROLE"

moveToWorkspace = composeAll [
    resource =?  "Pidgin"  --> doF (W.shift "IM")
    --className =? "rdesktop" --> doF (W.shift "8")
    --className =? "Thunderbird" --> doF (W.shift "MAIL")
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
    nonFloatingWindows <+>
    ignoredWindows <+>
    moveToWorkspace <+>
    manageScratchPad <+>
    noFocusWindows

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook = fullscreenEventHook  <+> docksEventHook

myLogHook pipe = do
    dynamicLogWithPP $ statusInfo pipe
    fadeInactiveLogHook 0.7

statusInfo pipe = defaultPP {
    ppCurrent           = dzenColor "lightblue" "#0000aa",
    ppVisible           = wrap "(" ")",
    ppHidden            = \i -> case i of
        "NSP" -> ""
        _     -> i,
    ppHiddenNoWindows   = const "",
    ppUrgent            = dzenColor "yellow" "red",
    ppSep               = " | ",
    ppWsSep             = " ",
    ppTitle             = dzenColor "#7777ff" "" . shorten 30,
    ppOrder             =  \(ws:_:_:rest) -> ws : rest,
    ppOutput            = hPutStrLn pipe
    }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
myStartupHook = setWMName "LG3D" -- LG3D is to run some java apps properly (intellij for instance)
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
                  dzenWithArgs ( " (go to " ++ index++")") a d
myDzenUrgencyHook = MyUrgencyHook {dur = seconds 5, argss = ["-bg", "cyan","-fg","red", "-xs", "1", "-x", "100", "-w", "300"] }

myUrgencyConfig = urgencyConfig { suppressWhen = Visible, remindWhen = Every 20}
myUrgencyHook = withUrgencyHookC myDzenUrgencyHook myUrgencyConfig



main = do
    spawn "~/.dotfiles/dzen.sh | dzen2 -dock -xs 1 -x 500 -p  -ta r -expand \"r\""
    dzenPipe <- spawnPipe "dzen2 -dock -xs 1 -ta \"l\" -w 480 "
    _ <- spawn myTerminal
    xmonad $ docks $ myUrgencyHook $ defaults dzenPipe

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
