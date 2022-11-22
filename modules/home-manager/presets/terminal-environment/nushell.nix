{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.presets.nushell;
  starship-completions =
    pkgs.runCommand "starship-init" { buildInputs = [ pkgs.starship ]; } ''
      HOME="$(mktemp -d)" starship init nu > $out
    '';

  nushell-env-file = {
    executable = true;
    source = ../../../../config/nushell/env.nu;
  };

  nushell-config-file = {
    executable = true;
    text = ''
      source ${../../../../config/nushell/config.nu}
      source ${starship-completions}
    '';
  };

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
    xdg = optionalAttrs (pkgs.stdenv.isDarwin == false) {
      configFile."nushell/config.nu" = nushell-config-file;
      configFile."nushell/env.nu" = nushell-env-file;
    };

    # Why they gotta make it so hard.
    home.file = optionalAttrs (pkgs.stdenv.isDarwin) {
      "Library/Application Support/nushell/config.nu" = nushell-config-file;
      "Library/Application Support/nushell/env.nu" = nushell-env-file;
    };
  };
}
