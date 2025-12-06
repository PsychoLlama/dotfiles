{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) types mkOption mkEnableOption;

  # Type accepted by `generators.toLua`. Similar to `pkgs.formats.*`.
  luaValueType =
    let
      valueType =
        types.nullOr (
          types.oneOf [
            types.bool
            types.int
            types.float
            types.str
            (types.attrsOf valueType)
            (types.listOf valueType)
          ]
        )
        // {
          description = "Lua value";
        };
    in
    valueType;

  # An attrset of only the enabled plugins. { vim-fugitive = <derivation>; }
  enabledVimPlugins = lib.filterAttrs (k: v: v.enable) config.plugins;

  # A list of the enabled plugins. [ <derivation> ]
  managedPackages = lib.mapAttrsToList (_: plugin: plugin.package) enabledVimPlugins;

  # Resolve a plugin config to a store path. Accepts either a path to a Lua
  # file or inline Lua code as a string.
  resolveConfig =
    plugin:
    if plugin.extraConfig == null then
      null
    else if lib.isPath plugin.extraConfig then
      # Use builtins.path to isolate the file so unrelated repo changes don't
      # invalidate the editor derivation.
      builtins.path {
        name = "${plugin.package.pname}-config.lua";
        path = plugin.extraConfig;
      }
    else
      toString (pkgs.writeText "${plugin.package.pname}-config.lua" plugin.extraConfig);

  # Managed plugins have more features, such as an associated config file.
  managedManifest = lib.mapAttrsToList (_: plugin: {
    name = plugin.package.pname;
    source = plugin.package.outPath;
    opts = plugin.opts;
    type = "pack";
    config = resolveConfig plugin;
  }) enabledVimPlugins;

  # Unmanaged plugins have less information. They are any derivation that
  # (hopefully) evaluates to a vim plugin.
  unmanagedManifest = lib.forEach config.extraPlugins (pkg: {
    name = pkg.pname;
    source = pkg.outPath;
    type = "pack";
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
            package = lib.mkPackageOption pkgs.vimPlugins name { };
            extraConfig = mkOption {
              type = types.nullOr (types.either types.path types.lines);
              default = null;
              description = ''
                Plugin specific config file.
                Only runs if the plugin is enabled and after it loads.
              '';
            };

            opts = mkOption {
              type = luaValueType;
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
