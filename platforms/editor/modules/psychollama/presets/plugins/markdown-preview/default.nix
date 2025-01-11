{ lib, config, ... }:

let
  cfg = config.psychollama.presets.plugins.markdown-preview-nvim;
in

{
  config.plugins.markdown-preview-nvim = lib.mkIf cfg.enable {
    opts.browser = lib.mkDefault "firefox";
  };
}
