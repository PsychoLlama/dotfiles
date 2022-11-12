{ config, lib, inputs, ... }:

with lib;

let cfg = config.dotfiles;

in {
  options.dotfiles = {
    packageSet = mkOption {
      type = types.enum [ "nixpkgs" "nixpkgs-unstable" ];
      description = "Change the default package set for all dotfiles";
      default = "nixpkgs";
    };
  };

  config.nixpkgs.overlays = [
    (if cfg.packageSet == "nixpkgs-unstable" then
      inputs.self.overlays.latest-packages
    else
      (self: pkgs: { unstable = pkgs; }))
  ] ++ [ inputs.self.overlays.vim-plugins ];
}
