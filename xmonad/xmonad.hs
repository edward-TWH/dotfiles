import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.StackSet as W
import XMonad.Util.Loggers

import XMonad.Util.SpawnOnce

import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.TaffybarPagerHints

import XMonad.Util.Themes
import XMonad.Layout.SimpleDecoration

import Graphics.X11.ExtraTypes.XF86

-- Variables
myMod :: KeyMask
myMod = mod4Mask -- Super key as mod

myTerminal :: String
myTerminal = "alacritty"

mySpacing = spacing 10

myFocusedBorderColour = "#fe8019"

myBorderWidth = 3


--

main :: IO ()
main = xmonad 
     $ ewmhFullscreen 
     $ ewmh 
     $ withEasySB (statusBarProp "taffybar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig


myStartupHook :: X ()
myStartupHook = do 
  spawn "autorandr -c"
  spawn "nitrogen --restore"

  spawnOnce "picom" 
  spawnOnce "nm-tray"
  spawnOnce "nm-applet"
  spawnOnce "blueman-applet"
  spawnOnce "dunst"
  spawnOnce "alarm-clock-applet --hidden"


myLayout = smartBorders tiled ||| noBorders Full 
  where
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 50/100    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes


myConfig = def
    { modMask  = myMod  
    , terminal = myTerminal
    , layoutHook = mySpacing $ myLayout
    , startupHook = myStartupHook
    , focusedBorderColor = myFocusedBorderColour
    , borderWidth = myBorderWidth
    }
    `additionalKeysP`
    [ ("M-q", kill)
    , ("M-C-r", restart "xmonad" True)
    , ("M-<Return>", spawn myTerminal)
    , ("M-m", windows W.swapMaster)
    , ("M-d", spawn "j4-dmenu-desktop")
    , ("M-e", spawn ("thunar"))
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ("<XF86AudioMicMute>", spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%")
    ]

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
