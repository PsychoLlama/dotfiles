{ lib, config, ... }:

let
  cfg = config.psychollama.presets.plugins.snacks-nvim;
in

{
  config.plugins.snacks-nvim = lib.mkIf cfg.enable {
    opts = {
      input.enabled = true;
      picker = {
        enabled = true;
        layout = {
          fullscreen = true;
          preset = "telescope";
        };
      };
    };
  };
}
