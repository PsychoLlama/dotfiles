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
    ./modules/rust-development.nix
    ./modules/js-development.nix
    ./modules/infrastructure.nix
    ./modules/system.nix
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
    environment = { etc.gitconfig.source = ./config/git.ini; };

    # Create a personal user profile.
    users.users.${config.dotfiles.user.account} = {
      isNormalUser = true;
      description = config.dotfiles.user.fullName;
      extraGroups = [ "wheel" ];

      packages = with unstable; [
        # Misc Language Tools
        nixfmt
        shellcheck
        vim-vint

        # Tools
        git
        gitAndTools.delta
        miniserve
      ];
    };
  };
}
