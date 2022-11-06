{ config, lib, ... }:

let cfg = config.presets.zoxide;

in with lib; {
  options.presets.zoxide.enable =
    mkEnableOption "Use Zoxide to jump around directories";

  # Pretty vanilla.
  config.programs.zoxide.enable = cfg.enable;
}
