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
    opts = plugin.opts;
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
    opts = { };
  });
in
{
  options.plugins = mkOption {
    description = "Plugins to install indexed by name";
    type = types.attrsOf (
      types.submodule (
        { name, ... }:
        {
          options = {
            name = mkOption {
              type = types.str;
              description = "Plugin name. Defaults to a member of `vimPlugins`.";
              default = name;
            };

            enable = mkEnableOption "Install ${name}";
            package = mkPackageOption pkgs.vimPlugins name { };
            extraConfig = mkOption {
              type = types.either types.path types.lines;
              default = "";
              description = ''
                Plugin specific config file.
                Only runs if the plugin is enabled and after it loads.
              '';
            };

            opts = mkOption {
              type = types.attrsOf types.anything;
              default = { };
              description = "Options passed to the config if it returns a function";
            };
          };
        }
      )
    );
  };

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
