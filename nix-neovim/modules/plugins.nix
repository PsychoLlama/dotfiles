{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  # An attrset of only the enabled plugins. { vim-fugitive = <derivation>; }
  enabledVimPluginSet = filterAttrs (k: v: v.enable) config.plugins;

  # A list of the enabled plugins. [ <derivation> ]
  enabledPluginPackages = mapAttrsToList (
    pluginName: _: pkgs.vimPlugins.${pluginName}
  ) enabledVimPluginSet;

  # Attrset of enabled plugins that define a config.
  enabledPluginsWithConfig = filterAttrs (
    pluginName: plugin: plugin.extraConfig != ""
  ) enabledVimPluginSet;

  # `:source` commands for every plugin that defines a config.
  enabledPluginConfigs = concatMapStringsSep "\n" (config: "vim.cmd.source('${config}')") (
    mapAttrsToList (
      pluginName: plugin: pkgs.writeText "${pluginName}-config.lua" plugin.extraConfig
    ) enabledPluginsWithConfig
  );
in
{
  imports = [ ./plugins ];

  # Generate an option for every vim plugin.
  options.plugins = mapAttrs' (pluginName: plugin: {
    name = pluginName;
    value = {
      enable = mkEnableOption "Add ${pluginName}";
      extraConfig = mkOption {
        type = types.lines;
        description = "Extra lines for the vim config file";
        default = "";
      };
    };
  }) pkgs.vimPlugins;

  config = {
    extraPlugins = enabledPluginPackages;
    extraConfig = enabledPluginConfigs;
  };
}
