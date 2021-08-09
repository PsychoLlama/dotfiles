{ config, lib, pkgs, unstable, inputs, ... }:

{
  imports = [
    ./modules/editor.nix
    ./modules/chat-client.nix
    ./modules/dev-shell.nix
    ./modules/networking.nix
    ./modules/desktop.nix
    ./modules/passwordless-sudo.nix
    ./modules/apps.nix
    ./modules/files.nix
  ];

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
    # Set up the global environment.
    environment = {
      etc.gitconfig.source = ./config/git.ini;

      variables = {
        # Provides dependencies for common Rust libraries.
        PKG_CONFIG_PATH = "${unstable.openssl.dev}/lib/pkgconfig";
      };
    };

    # Create a personal user profile.
    users.users.${config.dotfiles.user.account} = {
      isNormalUser = true;
      description = config.dotfiles.user.fullName;
      extraGroups = [ "wheel" "docker" ];

      packages = with unstable; [
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
        ipfs
        miniserve
        pastel

        (callPackage ./pkgs/w3m.nix { keymap = ./config/w3m.keymap; })
      ];
    };
  };
}
