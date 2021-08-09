{ config, unstable, lib, ... }:

let cfg = config.dotfiles.toolkit.files;

in {
  options.dotfiles.toolkit.files = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable file navigation/inspection toolkit";
      default = true;
    };

    replace = mkOption {
      type = types.bool;
      description = "Replaces cat & ls with fancier alternatives";
      default = true;
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
