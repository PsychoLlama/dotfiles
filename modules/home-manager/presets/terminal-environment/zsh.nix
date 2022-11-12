{ config, lib, ... }:

with lib;

let cfg = config.presets.zsh;

in {
  options.presets.zsh.enable =
    mkEnableOption "Use an opinionated Zsh configuration";

  config.programs.zsh = mkIf cfg.enable {
    enable = true;
    enableAutosuggestions = mkDefault true;
    enableSyntaxHighlighting = mkDefault true;
    defaultKeymap = "viins";

    # Essentially, never delete shell history.
    history = rec {
      size = 1000000;
      save = size;
    };

    # TODO: Replace as much of this as possible with declarative alternatives.
    initExtra = ''
      source ${../../../../config/init.zsh}
    '';
  };
}
