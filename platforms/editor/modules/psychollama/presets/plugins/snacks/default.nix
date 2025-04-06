{ lib, config, ... }:

let
  cfg = config.psychollama.presets.plugins.snacks-nvim;
in

{
  config.plugins.snacks-nvim = lib.mkIf cfg.enable {
    opts = {
      input.enabled = true;

      styles.float = {
        row = 75;
        height = 1.0;
        width = 1.0;
      };
    };
  };
}
