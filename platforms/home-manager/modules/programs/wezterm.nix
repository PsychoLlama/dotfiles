{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.wezterm;
  json = pkgs.formats.json { };
in

{
  options.programs.wezterm = {
    settings = lib.mkOption {
      type = json.type;
      default = { };
      description = "Settings injected into the WezTerm config as JSON";
    };

    configFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Lua config file that receives the settings";
    };
  };

  config.programs.wezterm.extraConfig = lib.mkIf cfg.enable (
    lib.mkMerge [
      ''
        local nix = ${builtins.toJSON cfg.settings}
      ''

      (lib.mkIf (cfg.configFile != null) ''
        return dofile('${cfg.configFile}')
      '')
    ]
  );
}
