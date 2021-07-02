import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig (additionalKeys, removeKeys)

-- Adapted from taylor1791/dotfiles.

main :: IO ()
main = do
  xmonad =<<
    statusBar "xmobar" xmonadBar toggleKey xmonadConfig


-- Contents of XMonad portion of xmobar.
xmonadBar :: PP
xmonadBar = xmobarPP {
  ppCurrent = xmobarColor "#C678DD" "" . wrap "[" "]",
  ppSep = xmobarColor "#5C6370" "" " : ",
  ppTitle = xmobarColor "#C678DD" ""
}


-- Keybinding to toggle XMonad covering the status bar.
toggleKey :: XConfig Layout -> (KeyMask, KeySym)
toggleKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


xmonadConfig = def {
    -- Every session should run tmux.
    terminal = "alacritty -e tmux",

    -- OneDark Theme
    focusedBorderColor = "#E5C07B", -- Yellow
    normalBorderColor  = "#4B5263"  -- Gutter Grey
  } `removeKeys` [
      (mod1Mask, xK_p) -- Remove dmenu for rofi
    , (shiftMask .|. mod1Mask, xK_p) -- Remove dmrun for rofi-calc

    -- Remove player controls
    , ((0, xK_Home))
    , ((0, xK_End))
    , ((0, xK_Insert))
  ] `additionalKeys` [
    -- Run laucher
    ((mod1Mask, xK_p), spawn "rofi -show drun -display-drun 'Start: '"),

    -- Lock the computer
    ((mod1Mask, xK_s), spawn "slock"),

    -- Expose music controls
    ((0, xK_Home), spawn "playerctl previous"),
    ((0, xK_End), spawn "playerctl next"),
    ((0, xK_Insert), spawn "playerctl play-pause")
  ]
