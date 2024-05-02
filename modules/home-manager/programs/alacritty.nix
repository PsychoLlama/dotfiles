{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.alacritty;
  tomlFormat = pkgs.formats.toml { };
  settingsFile = tomlFormat.generate "alacritty.toml" cfg.settings;
in
{
  # Alacritty v0.13 replaced the Yaml file with TOML. Boo.
  config.xdg.configFile."alacritty/alacritty.toml".source = mkIf (cfg.settings != { }) settingsFile;
}
