{ config, pkgs, nixpkgs-unstable, lib, ... }:

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
      default = df.kitchen-sink.enable;
    };

    zsh = {
      rc = mkOption {
        type = types.path;
        description = "Set the zsh RC file";
        default = ../config/init.zsh;
      };

      extraConfig = mkOption {
        type = types.str;
        description = "Extra lines to append to the zshrc";
        default = "# => unset";
      };
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
      default = df.kitchen-sink.enable;
    };
  };

  config = with lib;
    mkMerge [
      (mkIf cfg.enable {
        environment.variables.STARSHIP_CONFIG = "${cfg.starship.config}";
        environment.etc."zshrc.local".text = ''
          # --- dotfiles.dev-shell.zsh.rc ---
          source ${cfg.zsh.rc}

          # --- dotfiles.dev-shell.zsh.extraConfig ---
          ${cfg.zsh.extraConfig}
        '';

        environment.systemPackages = [
          (nixpkgs-unstable.callPackage ../pkgs/alacritty.nix {
            configFile = cfg.alacritty.config;
          })
          nixpkgs-unstable.starship
        ];

        programs.tmux = {
          enable = true;
          keyMode = mkDefault "vi";
          escapeTime = mkDefault 0;
          historyLimit = mkDefault 10000;
          customPaneNavigationAndResize = mkDefault true;
          extraConfig = builtins.readFile cfg.tmux.config;
        };

        programs.zsh = {
          enable = true;
          syntaxHighlighting.enable = mkDefault true;
          autosuggestions.enable = mkDefault true;
          histSize = mkDefault 10000;
          promptInit = ''
            eval "$(starship init zsh)"
            eval "$(zoxide init zsh)"
          '';
        };

        users.users.${df.user.account}.shell = pkgs.zsh;

        fonts.fonts = [ nixpkgs-unstable.fira-code ];
        console.font = "Fira Code";
      })

      (mkIf (cfg.enable && cfg.tmux.aliases.enable) {
        environment.shellAliases = { t = "tmux"; };
      })
    ];
}
