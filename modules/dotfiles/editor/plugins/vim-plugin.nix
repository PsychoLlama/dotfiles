name: spec:

# A preset whose whole job is putting one plugin in the manifest: install it,
# optionally with a Lua config file and a defer trigger. A plain `.nix`
# helper, not a module: discovery is per-file, so each plugin still needs its
# own `mod.nix` to stay individually enableable, and this keeps thirty-odd of
# those from being thirty-odd copies of the same eight lines.
#
# `spec` is spliced into the plugin's manifest entry. Pass a function of
# `{ lib, pkgs }` when the settings need store paths. A plugin that has to
# reach outside its own manifest entry should stop calling this and spell
# itself out.

{
  cfg,
  lib,
  pkgs,
  ...
}:

let
  settings = if lib.isFunction spec then spec { inherit lib pkgs; } else spec;
in

{
  options.package = lib.mkOption {
    type = lib.types.nullOr lib.types.package;
    default = null;
    defaultText = lib.literalExpression "plugin.pkgs.${name}";
    description = ''
      Plugin package to install. Null resolves `${name}` by name against the
      editor's `plugin.sources`.
    '';
  };

  # `plugin.pkgs` only exists in the editor evaluation, so the by-name
  # fallback has to be resolved down there rather than in the option default.
  modules.editor =
    { config, ... }:

    {
      plugins.${name} = settings // {
        enable = lib.mkDefault true;
        package = lib.mkDefault (if cfg.package == null then config.plugin.pkgs.${name} else cfg.package);
      };
    };
}
