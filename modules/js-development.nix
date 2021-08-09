{ config, unstable, lib, ... }:

let cfg = config.dotfiles.toolkit.js-development;

in {
  options.dotfiles.toolkit.js-development = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable toolkit for JavaScript development";
      default = true;
    };
  };

  config = with lib; {
    environment.systemPackages = with unstable;
      mkIf cfg.enable [ nodejs pastel yarn ];
  };
}
