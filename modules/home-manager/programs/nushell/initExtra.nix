{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.programs.nushell;

  nushellConfigFile = {
    executable = true;
    text = cfg.initExtra;
  };

in {
  options.programs.nushell.initExtra = mkOption {
    type = types.lines;
    description = "Extra commands to run when starting the shell";
    default = "";
  };

  config = mkIf (cfg.enable && cfg.initExtra != "") {
    xdg = optionalAttrs (pkgs.stdenv.isDarwin == false) {
      configFile."nushell/config.nu" = nushellConfigFile;
    };

    # Why they gotta make it so hard.
    home.file = optionalAttrs (pkgs.stdenv.isDarwin) {
      "Library/Application Support/nushell/config.nu" = nushellConfigFile;
    };
  };
}
