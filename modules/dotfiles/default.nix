{ config, lib, inputs, ... }:

with lib;

let cfg = config.dotfiles;

in {
  options.dotfiles = {
    kitchen-sink.enable = mkOption {
      type = types.bool;
      description = "Enable everything";
      default = false;
    };

    package-set = mkOption {
      type = types.enum [ "nixpkgs" "nixpkgs-unstable" ];
      description = "Change the default package set for all dotfiles";
      default = "nixpkgs";
    };
  };

  config = mkMerge [
    {
      home-manager = {
        useGlobalPkgs = mkDefault true;
        useUserPackages = mkDefault true;
      };
    }

    ({
      nixpkgs.overlays = [
        (if cfg.package-set == "nixpkgs-unstable" then
          inputs.self.overlays.latest-packages
        else
          (self: pkgs: { unstable = pkgs; }))
      ];
    })

    { nixpkgs.overlays = [ inputs.self.overlays.vim-plugins ]; }
  ];
}
