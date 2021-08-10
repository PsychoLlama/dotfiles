{ config, pkgs, unstable, lib, ... }:

# dev-shell
#
# Configures everything you need to launch a fancy development shell.

let
  df = config.dotfiles;
  cfg = df.dev-shell;

in {
  options.dotfiles.dev-shell = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable a fancy development shell";
      default = true;
    };

    zsh.rc = mkOption {
      type = types.path;
      description = "Set the zsh RC file";
      default = ../config/init.zsh;
    };

    alacritty.config = mkOption {
      type = types.path;
      description = "Set the alacritty config file";
      default = ../config/alacritty.yml;
    };

    starship.config = mkOption {
      type = types.path;
      description = "Set the starship config file";
      default = ../config/starship.toml;
    };

    tmux.config = mkOption {
      type = types.path;
      description = "Set the tmux config file";
      default = ../config/tmux.conf;
    };

    tmux.aliases.enable = mkOption {
      type = types.bool;
      description = "Define some aliases for tmux";
      default = true;
    };
  };

  config = with lib; {
    environment.etc."zshrc.local".source = mkIf cfg.enable cfg.zsh.rc;

    environment.variables.STARSHIP_CONFIG =
      mkIf cfg.enable "${cfg.starship.config}";

    environment.systemPackages = mkIf cfg.enable [
      (unstable.callPackage ../pkgs/alacritty.nix {
        configFile = cfg.alacritty.config;
      })
      unstable.starship
    ];

    programs.tmux = mkIf cfg.enable {
      enable = true;
      keyMode = mkDefault "vi";
      escapeTime = mkDefault 0;
      historyLimit = mkDefault 10000;
      customPaneNavigationAndResize = mkDefault true;
      extraConfig = builtins.readFile cfg.tmux.config;
    };

    programs.zsh = mkIf cfg.enable {
      enable = true;
      syntaxHighlighting.enable = mkDefault true;
      autosuggestions.enable = mkDefault true;
      histSize = mkDefault 10000;
    };

    environment.shellAliases =
      mkIf (cfg.enable && cfg.tmux.aliases.enable) { t = "tmux"; };

    users.users.${df.user.account}.shell = mkIf cfg.enable pkgs.zsh;

    fonts.fonts = mkIf cfg.enable [ unstable.fira-code ];
    console.font = mkIf cfg.enable "Fira Code";
  };
}
