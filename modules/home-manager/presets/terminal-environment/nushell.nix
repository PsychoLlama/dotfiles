{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.presets.nushell;
  starship-completions =
    pkgs.runCommand "starship-init" { buildInputs = [ pkgs.starship ]; } ''
      HOME="$(mktemp -d)" starship init nu > $out
    '';

in {
  options.presets.nushell.enable =
    mkEnableOption "Install and configure Nushell";

  config = mkIf cfg.enable {
    programs.nushell = {
      enable = true;
      package = pkgs.unstable.nushell;

      # By default, this symlinks to the wrong location with the wrong format.
      # Use nulang config files instead.
      settings = { };
    };

    # TODO: Make this all configurable from outside the preset.
    xdg = {
      configFile."nushell/config.nu" = {
        executable = true;
        text = ''
          source ${../../../../config/nushell/config.nu}
          source ${starship-completions}
        '';
      };

      configFile."nushell/env.nu" = {
        executable = true;
        source = ../../../../config/nushell/env.nu;
      };
    };
  };
}
