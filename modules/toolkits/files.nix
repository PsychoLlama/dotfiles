{ config, unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.toolkit.files;

in {
  options.dotfiles.toolkit.files = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable file navigation/inspection toolkit";
      default = df.kitchen-sink.enable;
    };

    replace = mkOption {
      type = types.bool;
      description = "Replaces cat & ls with fancier alternatives";
      default = df.kitchen-sink.enable;
    };
  };

  config = with lib; {
    environment.variables = mkIf cfg.enable {
      SKIM_DEFAULT_COMMAND = "fd";
      BAT_THEME = "TwoDark";
      BAT_STYLE = "changes";
    };

    environment.shellAliases = mkIf (cfg.enable && cfg.replace) {
      cat = "bat";
      ls = "exa";
      l = "exa -la";
    };

    environment.systemPackages = with unstable;
      mkIf cfg.enable [
        bat
        binutils
        du-dust
        exa
        fd
        glow
        hexyl
        ipfs
        jq
        pv
        ripgrep
        skim
        tokei
        viu
        zoxide
      ];
  };
}
