local wezterm = require('wezterm')

local config = wezterm.config_builder()

-- Import color scheme from Nix config.
config.color_schemes = {
  [nix.color_scheme] = nix.colors,
}

-- Apply settings from Nix.
config.term = nix.term
config.color_scheme = nix.color_scheme
config.window_background_opacity = nix.window_background_opacity
config.hide_tab_bar_if_only_one_tab = nix.hide_tab_bar_if_only_one_tab
config.font_size = nix.font_size
config.harfbuzz_features = nix.harfbuzz_features
config.window_padding = nix.window_padding
config.default_prog = nix.default_prog

-- Font configuration (requires wezterm API).
config.font = wezterm.font(nix.font.family, { weight = nix.font.weight })
config.font_rules = {
  {
    intensity = 'Half',
    font = wezterm.font(nix.font.family, {
      weight = nix.font.weight,
      foreground = nix.colors.brights[1],
    }),
  },
}

-- Keybindings (requires wezterm API).
config.keys = {
  { key = 'L', mods = 'CTRL', action = wezterm.action.ShowDebugOverlay },
  { key = 'C', mods = 'CTRL|SHIFT', action = wezterm.action.CopyTo('Clipboard') },
  { key = 'V', mods = 'CTRL|SHIFT', action = wezterm.action.PasteFrom('Clipboard') },
}

return config
