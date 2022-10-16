{ config, lib, ... }:

let cfg = config.dotfiles;

in {
  imports = [
    ./modules/apps.nix
    ./modules/desktop.nix
    ./modules/dev-shell.nix
    ./modules/editor.nix
    ./modules/passwordless-sudo.nix
    ./modules/wireless.nix
    ./modules/toolkits/development.nix
    ./modules/toolkits/files.nix
    ./modules/toolkits/infrastructure.nix
    ./modules/toolkits/js-development.nix
    ./modules/toolkits/networking.nix
    ./modules/toolkits/rust-development.nix
    ./modules/toolkits/system.nix
  ];

  options.dotfiles = with lib; {
    kitchen-sink.enable = mkOption {
      type = types.bool;
      description = "Enable everything";
      default = false;
    };

    user = {
      manage = mkOption {
        type = types.bool;
        description = "Whether to manage the user account";
        default = config.dotfiles.kitchen-sink.enable;
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

  config = with lib;
    mkIf cfg.user.manage {
      # Create a personal user profile. Other modules depend on this.
      users.users = {
        ${cfg.user.account} = {
          isNormalUser = true;
          description = cfg.user.fullName;
          extraGroups = [ "wheel" ];
        };
      };

      home-manager = {
        useGlobalPkgs = mkDefault true;
        useUserPackages = mkDefault true;
      };
    };
}
