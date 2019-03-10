import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.RotSlaves
import XMonad.Actions.WindowBringer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.ComboP
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Paste
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare

import System.IO

import qualified XMonad.StackSet as W

myManageHook = composeAll
   [ className     =? "Signal"      --> doShift "c:chat"
   , title =? "Deluge" --> doShift "e:rt"
   , title     =? "Sid Meier's Civilization: Beyond Earth" --> doFloat
   , manageDocks
   ] <+> (isFullscreen --> doFullFloat) <+> manageScratchpad

manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook(W.RationalRect l t w h)
                where
                        h = 1 / 3
                        w = 1 / 3
                        t = 0.33
                        l = 0.33


backgroundColor, textColor, color3, color4, color5 :: [Char]
backgroundColor = "#181818"
textColor = "#d8d8d8"
color5 = "#DC9656"
color4 = "#A1B56C"
color3 = "#AB4642"

lemonColor :: String -> String -> String -> String
lemonColor fgcolor bgcolor str = "%{F" ++ fgcolor ++ "}" ++ str -- ++ "%{B" ++ bgcolor ++ "}"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   lemonColor color3 backgroundColor . pad
      , ppVisible           =   lemonColor color5 backgroundColor . pad
      , ppHidden            =   lemonColor textColor backgroundColor . pad . noScratchpad
      , ppHiddenNoWindows   =   lemonColor textColor backgroundColor . pad . noScratchpad
      , ppUrgent            =   lemonColor color4 backgroundColor . pad
      , ppWsSep             =   ""
      , ppSep               =   " | "
      , ppLayout            =   lemonColor color3 backgroundColor
      , ppTitle             =   (" " ++) . lemonColor textColor backgroundColor . shorten 1000 . dzenEscape
      , ppOutput            =   System.IO.hPutStrLn h
    }
    where
        noScratchpad ws = if ws == "NSP" then "" else ws

myTerminal = "xfce4-terminal"

tabConfig' = def { decoWidth = 200
                 , decoHeight = 200
                 , fontName = "Tamsyn-11"
}

layout' = TwoPane (3/100) (1/2) ||| tall' ||| Full 
  where
    tall' = Tall nmaster delta ratio
    nmaster = 1
    ratio = 0.618034 -- Golden ratio with a + b = 1.
    delta = 3/100

wrap' :: [Char] -> [Char] -> [Char]
wrap' y x = y ++ x ++ y

wrapq = wrap' "\""

dmenu' = "dmenu_run -fn Tamsyn-11 -nb '" ++ backgroundColor ++ "' -nf '" ++ textColor ++ "' -sb '" ++ color3 ++ "' -sf '" ++ textColor ++ "'"
dmenu_args = ["-fn", "Tamsyn-11", "-nb", backgroundColor, "-nf", textColor, "-sb", color3, "-sf", textColor]
dmenu_cmd = "fish -c " ++ wrapq dmenu'
workspaceNames = ["misc",  "pdf", "patents", "meditation", "mpv", "game", "rt", "status", "anki", "music", "de", "emacs", "", "blog", "chat"]
workspaceKeys = ["1", "2", "3", "4", "q", "w", "e", "r", "a", "s", "d", "f", "z", "x", "c"]
workspaces' = zipWith (++) (map (\x -> x ++ ":") workspaceKeys) workspaceNames

main = do
  xmproc <- spawnPipe "xmobar -x 1 ~/.xmonad/xmobarrc"
  xmonad $ ewmh defaultConfig
            { terminal = myTerminal
            , focusFollowsMouse = True
            , modMask = mod4Mask
            , layoutHook = smartBorders $ avoidStruts $ smartSpacing 5 $ layout'
            , manageHook = manageHook defaultConfig <+> myManageHook
            , workspaces = workspaces'
            , borderWidth = 1
            , normalBorderColor = backgroundColor
            , focusedBorderColor = color3
            , logHook = dynamicLogWithPP $ xmobarPP
                       { ppOutput = hPutStrLn xmproc
                       , ppTitle = xmobarColor "#b0b0b0" "" . shorten 50
                       , ppCurrent = xmobarColor "#f4bf75" "" . shorten 50
                       , ppVisible = xmobarColor "#d28445" "" . shorten 50
                       , ppHidden = xmobarColor "#b0b0b0" "" . shorten 50
                       , ppHiddenNoWindows = xmobarColor "#b0b0b0" "" . shorten 50
                       }
            , handleEventHook = docksEventHook <+> handleEventHook  defaultConfig
            }
            `additionalMouseBindings`
            [ ((0, 8), \w -> prevWS ) -- Use mouse button to move to the previous workspace.
            , ((0, 9), \w -> nextWS ) -- Use mouse button to move to the next workspace.
            ]
            `additionalKeysP`
            ([ ("C-d q", spawn "surf-wrapper")
	    , ("C-d s", spawn dmenu_cmd)
            , ("M4-k", windows W.focusDown)                            -- ^
            , ("C-d k", windows W.focusDown)                      -- ^
            , ("C-d a", spawn "emacsclient -ne  '(make-agenda-frame)'")                          -- Show agenda. 
            , ("C-d d", spawn "emacsclient -ne '(make-capture-todo-frame)'")                     -- Capture TODO.
            , ("C-d z", spawn "emacsclient -ne '(make-capture-frame)'")                          -- Capture /something/.
            , ("M4-j", windows W.focusUp)                              -- ^
            , ("C-d j", windows W.focusUp)                         -- ^
            , ("C-d <Return>", windows W.swapMaster)                    -- Swap master window and focused window.
            , ("M4-<Up>", rotSlavesUp)                         -- Swap up. 
            , ("M4-<Down>", rotSlavesDown)                    -- Swap down. 
            , ("M4-h", sendMessage Shrink)                               -- Shrink the master area.
            , ("M4-l", sendMessage Expand)                               -- Grow the master area.
            , ("M4-<Left>", sendMessage Shrink)                               -- Shrink the master area.
            , ("M4-<Right>", sendMessage Expand)                               -- Grow the master area.
            , ("C-d x", kill)                                           -- Kill the selected window.
            , ("C-d c", spawn myTerminal)                                 -- Start terminal.
            , ("C-d <Space>", sendMessage NextLayout)                   -- Switch layout.
            , ("C-d n", windows W.swapDown)                             -- Swap focused window with next window.
            , ("C-d p", spawn "gopass completion dmenu")                -- Password manager.
            , ("C-d =", sendMessage (IncMasterN 1))                     -- Increment the number of windows in the master area.
            , ("C-d -", sendMessage (IncMasterN (-1)))                  -- Decrement the number of windows in the master area.
            , ("C-d 4", spawn "ruby ~/bin/clip.rb")                     -- OS X style screenshotting.
            , ("C-d t", withFocused $ windows . W.sink)                 -- Force app back into tiling.
            , ("C-d v", pasteSelection)                                 -- Pastes the x buffer clipboard.
            , ("C-d w", gotoMenuArgs dmenu_args )                                       -- Takes you to a window.
            , ("C-d l", spawn "slock")                                  -- Locks screen.
            , ("C-d r", spawn "frw")
            , ("<XF86MonBrightnessDown>", spawn "sudo xbacklight -dec 10") -- Enable screen brightness function key.
            , ("<XF86MonBrightnessUp>", spawn "sudo xbacklight -inc 10")    -- ,
            , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 1 +5%; pamixer --get-volume > ~/stats/vol")
            , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 1 -5%; pamixer --get-volume > ~/stats/vol")
            , ("<XF86AudioMute>", spawn "pactl set-sink-mute 1 toggle")
            , ("M4-<Space>", sendMessage NextLayout)
            , ("C-d ,", spawn "playerctl previous")
            , ("C-d .", spawn "playerctl next")
            , ("C-d /", spawn "playerctl play-pause")
            , ("M4-<Tab>", cycleRecentWS [xK_Super_L] xK_Tab xK_Tab)
            ]
            ++
            [ (otherModMasks ++ "M4-" ++ key, action tag)
            | (tag, key)  <- zip workspaces' workspaceKeys
            , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- or W.view
                                         , ("S-", windows . W.shift)]
            ])
