{ config, lib, pkgs, unstable, inputs, ... }:

{
  imports =
    [ ./modules/editor.nix ./modules/chat-client.nix ./modules/dev-shell.nix ];

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

    # Set up the global environment.
    environment = {
      etc.gitconfig.source = ./config/git.ini;

      variables = {
        SKIM_DEFAULT_COMMAND = "fd";
        BAT_THEME = "TwoDark";
        BAT_STYLE = "changes";

        # Provides dependencies for common Rust libraries.
        PKG_CONFIG_PATH = "${unstable.openssl.dev}/lib/pkgconfig";
      };

      systemPackages = with unstable; [
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

      packages = with unstable; [
        # Graphical Apps
        firefox
        torbrowser
        zathura

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
        ipmitool
        kubectl
        terraform_1_0_0

        # Misc Language Tools
        nixfmt
        shellcheck
        tokei
        vim-vint

        # Networking
        dogdns
        nmap
        termshark
        whois
        xh

        # File Management and Navigation
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
        viu
        zoxide

        # System
        acpi
        bottom
        ncspot
        playerctl
        rage
        scrot
        xclip

        # Tools
        git
        gitAndTools.delta
        miniserve
        pastel
      ];
    };
  };
}
