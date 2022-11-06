{ config, lib, ... }:

let cfg = config.presets.zsh;

in with lib; {
  options.presets.zsh.enable =
    mkEnableOption "Use an opinionated Zsh configuration";

  config = mkIf cfg.enable {
    programs.zsh = {
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

    # TODO: Move these into their respective configs, once they exist.
    home.shellAliases = {
      t = "tmux";
      cat = "bat";

      g = "git";
      c = "git commit";
      b = "git branch";
      ch = "git checkout";
      h = "git diff HEAD";
      hh = "git diff HEAD~1";
      hhh = "git diff HEAD~2";
    };
  };
}
