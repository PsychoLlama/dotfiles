{ config, lib, pkgs, unstable, inputs, ... }:

{
  imports = [ ./modules/editor.nix ];

  options.dotfiles = with lib; {
    user.account = mkOption {
      type = types.str;
      example = "ealderson";
      description = "Your username";
    };

    user.fullName = mkOption {
      type = types.str;
      example = "Elliot Alderson";
      description = ''
        Short description of the user account, usually your full name.
      '';
    };
  };

  config = {
    # Enable the X11 windowing system.
    services.xserver = {
      enable = true;

      # Seems a more reasonable default.
      autoRepeatDelay = lib.mkDefault 250;

      libinput = {
        enable = true;

        # Configure the touchpad.
        touchpad = {
          naturalScrolling = lib.mkDefault true;
          tapping = lib.mkDefault false; # Disable soft tap to click.
        };
      };

      # Swap out the login screen program.
      displayManager.lightdm.greeters.enso.enable = true;

      # Use XMonad to manage the graphical environment.
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ./config/xmonad.hs;
      };
    };

    # System programs.
    programs.wireshark.enable = true;
    programs.slock.enable = true; # Screen locking utility.

    programs.zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
      autosuggestions.enable = true;
      histSize = 10000;

      shellAliases = {
        cat = "bat";
        ls = "exa";
        l = "exa -la";

        t = "tmux";
        ":qa" = "tmux kill-session 2> /dev/null || exit";

        g = "git";
        c = "git commit";
        b = "git branch";
        ch = "git checkout";
        h = "git diff HEAD";
        hh = "git diff HEAD~1";
        hhh = "git diff HEAD~2";

        ".." = "cd ..";
        "..." = "cd ../..";
        "...." = "cd ../../..";
      };
    };

    programs.tmux = {
      enable = true;
      keyMode = "vi";
      escapeTime = 0;
      historyLimit = 10000;
      customPaneNavigationAndResize = true;
      extraConfig = builtins.readFile ./config/tmux.conf;
    };

    # Set up the global environment.
    environment = {
      etc."zshrc.local".source = ./config/init.zsh;
      etc.gitconfig.source = ./config/git.ini;

      variables = {
        SKIM_DEFAULT_COMMAND = "fd";
        BAT_THEME = "TwoDark";
        BAT_STYLE = "changes";
        STARSHIP_CONFIG = "${./config/starship.toml}";

        # Provides dependencies for common Rust libraries.
        PKG_CONFIG_PATH = "${unstable.openssl.dev}/lib/pkgconfig";
      };

      systemPackages = with unstable; [
        (callPackage ./pkgs/alacritty.nix {
          configFile = ./config/alacritty.yml;
        })

        (callPackage ./pkgs/rofi.nix { configDir = ./config/rofi; })

        (callPackage ./pkgs/w3m.nix { keymap = ./config/w3m.keymap; })
      ];
    };

    users.groups.pantheon = { };
    security.sudo.extraRules = [{
      groups = [ "pantheon" ];
      commands = [{
        command = "ALL";
        options = [ "NOPASSWD" ];
      }];
    }];

    # Create a personal user profile.
    users.users.${config.dotfiles.user.account} = {
      isNormalUser = true;
      description = config.dotfiles.user.fullName;
      extraGroups = [ "wheel" "docker" "pantheon" ];
      shell = pkgs.zsh;

      packages = with unstable; [
        # Graphical Apps
        element-desktop
        firefox
        torbrowser
        zathura

        # Terminal Apps
        aerc
        ncspot
        termshark

        # Rust Development
        cargo
        cargo-edit
        clippy
        gcc
        openssl.dev
        pkg-config
        rls
        rustc
        rustup

        # JS Development
        nodejs
        yarn

        # Infrastructure
        ansible
        ipmitool
        kubectl
        terraform_1_0_0

        # Tools
        acpi
        bat
        binutils
        bottom
        dogdns
        du-dust
        exa
        fd
        git
        gitAndTools.delta
        glow
        hexyl
        ipfs
        jq
        miniserve
        nixfmt
        nmap
        pastel
        playerctl
        pv
        rage
        ripgrep
        scrot
        shellcheck
        skim
        starship
        tokei
        vim-vint
        viu
        whois
        xclip
        xh
        zoxide

        # Chat client
        (weechat.override {
          configure = { ... }: {
            scripts = with weechatScripts; [ wee-slack weechat-matrix ];
          };
        })
      ];
    };
  };
}
