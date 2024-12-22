{ lib, config, ... }:

let
  cfg = config.presets.plugins.markdown-preview-nvim;
in

{
  config.plugins.markdown-preview-nvim = lib.mkIf cfg.enable {
    opts.browser = lib.mkDefault "firefox";
  };
}
