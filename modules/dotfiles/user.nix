{ config, lib, options, pkgs, ... }:

with lib;

let cfg = config.dotfiles.user;

in {
  # Create a personal user profile. Other modules depend on this.
  options.dotfiles.user = mkOption {
    type = types.nullOr types.attrs;
    description = "The primary user account";
  };

  config = mkMerge [
    { users.users.${cfg.name} = mkAliasDefinitions options.dotfiles.user; }

    (mkIf (cfg != null && pkgs.stdenv.isDarwin == false) {
      users.users.${cfg.name}.isNormalUser = mkDefault true;
    })
  ];
}
