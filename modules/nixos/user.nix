{ config, lib, options, ... }:

let cfg = config.dotfiles.user;

in with lib; {
  # Create a personal user profile. Other modules depend on this.
  options.dotfiles.user = mkOption {
    type = types.nullOr types.attrs;
    description = "The primary user account";
    default = null;
  };

  config = mkMerge [
    (mkIf (cfg != null) {
      users.users.${cfg.name} = mkAliasDefinitions options.dotfiles.user;
    })

    (mkIf (cfg != null) {
      users.users.${cfg.name}.isNormalUser = mkDefault true;
    })
  ];
}
