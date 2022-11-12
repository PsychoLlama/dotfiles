{ config, lib, inputs, ... }:

let cfg = config.dotfiles;

in {
  options.dotfiles = with lib; {
    kitchen-sink.enable = mkOption {
      type = types.bool;
      description = "Enable everything";
      default = false;
    };

    bleeding-edge = mkOption {
      type = types.bool;
      description = "Use the latest packages";
      default = cfg.kitchen-sink.enable;
    };
  };

  config = with lib;
    mkMerge [
      {
        home-manager = {
          useGlobalPkgs = mkDefault true;
          useUserPackages = mkDefault true;
        };
      }

      ({
        nixpkgs.overlays = [
          (if cfg.bleeding-edge then
            inputs.self.overlays.latest-packages
          else
            (self: pkgs: { unstable = pkgs; }))
        ];
      })

      { nixpkgs.overlays = [ inputs.self.overlays.vim-plugins ]; }
    ];
}
