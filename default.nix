{ config, lib, ... }:

let cfg = config.dotfiles;

in {
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
    ./modules/development.nix
  ];

  options.dotfiles = with lib; {
    user = {
      manage = mkOption {
        type = types.bool;
        description = "Whether to manage the user account";
        default = true;
      };

      account = mkOption {
        type = types.str;
        example = "ealderson";
        description = "Your username";
      };

      fullName = mkOption {
        type = types.str;
        example = "Elliot Alderson";
        description = ''
          Short description of the user account, usually your full name.
        '';
      };
    };
  };

  config = with lib; {
    # Create a personal user profile. Other modules depend on this.
    users.users = mkIf cfg.user.manage {
      ${cfg.user.account} = {
        isNormalUser = true;
        description = cfg.user.fullName;
        extraGroups = [ "wheel" ];
      };
    };
  };
}
