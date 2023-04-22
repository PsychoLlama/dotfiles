{ config, lib, pkgs, ... }:

with lib;

let
  # An attrset of only the enabled plugins. { vim-fugitive = <derivation>; }
  enabledVimPluginSet = filterAttrs (k: v: v.enable) config.plugins;

  # A list of the enabled plugins. [ <derivation> ]
  enabledPluginPackages =
    mapAttrsToList (pluginName: _: pkgs.vimPlugins.${pluginName})
    enabledVimPluginSet;

in {
  imports = [ ./plugins/coc-nvim ];

  # Generate an option for every vim plugin.
  options.plugins = mapAttrs' (pluginName: plugin: {
    name = pluginName;
    value.enable = mkEnableOption "Add ${pluginName}";
  }) pkgs.vimPlugins;

  config.extraPlugins = enabledPluginPackages;
}
