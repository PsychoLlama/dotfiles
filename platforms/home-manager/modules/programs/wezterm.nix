{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.wezterm;
  toLua = lib.generators.toLua { };
  json = pkgs.formats.json { };
in

{
  options.programs.wezterm = {
    # TODO: Replace this with a `wezterm.lua` file.
    # Generate variables, not programs.
    settings = lib.mkOption {
      type = json.type;
      default = { };
      description = "Generated WezTerm config";
    };
  };

  config.programs.wezterm.extraConfig = lib.mkIf cfg.enable ''
    return ${toLua cfg.settings};
  '';
}
