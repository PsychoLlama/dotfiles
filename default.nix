{ config, lib, ... }:

let cfg = config.dotfiles;

in {
  imports = [
    ./modules/apps.nix
    ./modules/chat-client.nix
    ./modules/desktop.nix
    ./modules/dev-shell.nix
    ./modules/editor.nix
    ./modules/passwordless-sudo.nix
    ./modules/toolkits/development.nix
    ./modules/toolkits/files.nix
    ./modules/toolkits/infrastructure.nix
    ./modules/toolkits/js-development.nix
    ./modules/toolkits/networking.nix
    ./modules/toolkits/rust-development.nix
    ./modules/toolkits/system.nix
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
