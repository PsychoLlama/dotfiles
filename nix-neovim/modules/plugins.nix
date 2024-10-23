{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  # An attrset of only the enabled plugins. { vim-fugitive = <derivation>; }
  enabledVimPlugins = filterAttrs (k: v: v.enable) config.plugins;

  # A list of the enabled plugins. [ <derivation> ]
  managedPackages = mapAttrsToList (_: plugin: plugin.package) enabledVimPlugins;

  # Managed plugins have more features, such as an associated config file.
  managedManifest = lib.mapAttrsToList (_: plugin: {
    name = plugin.package.pname;
    source = plugin.package.outPath;
    config = (
      if lib.isPath plugin.extraConfig then
        toString plugin.extraConfig
      else if plugin.extraConfig != "" then
        toString (pkgs.writeText "${plugin.package.pname}-config.lua" plugin.extraConfig)
      else
        null
    );
  }) enabledVimPlugins;

  # Unmanaged plugins have less information. They are any derivation that
  # (hopefully) evaluates to a vim plugin.
  unmanagedManifest = lib.forEach config.extraPlugins (pkg: {
    name = pkg.pname;
    source = pkg.outPath;
  });
in
{
  imports = [ ./plugins ];

  # Generate an option for every vim plugin. Not using a submodule because
  # plugins can be extended with custom Nix options and submodules require
  # uniform types.
  options.plugins = mapAttrs' (pluginName: plugin: {
    name = pluginName;
    value = {
      enable = mkEnableOption "Add ${pluginName}";
      package = mkPackageOption pkgs.vimPlugins pluginName { };
      extraConfig = mkOption {
        type = types.either types.path types.lines;
        description = "Extra lines for the vim config file";
        default = "";
      };
    };
  }) pkgs.vimPlugins;

  options.core.manifest = mkOption {
    type = types.anything;
    readOnly = true;
    internal = true;
    description = "Plugin manifest generated for the core framework";
    default = lib.sortOn (plugin: plugin.name) (managedManifest ++ unmanagedManifest);
  };

  options.core.packages = mkOption {
    type = types.listOf types.package;
    readOnly = true;
    internal = true;
    description = "Set of all packages to install as optionals in &packpath";
    default = managedPackages ++ config.extraPlugins;
  };
}
