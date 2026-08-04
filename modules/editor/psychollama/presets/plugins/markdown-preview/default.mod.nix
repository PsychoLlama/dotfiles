{ lib, config, ... }:

let
  cfg = config.psychollama.presets.plugins.markdown-preview-nvim;
in

{
  config = lib.mkIf cfg.enable {
    # markdown-preview-nvim runs a Node.js server to render the preview.
    withNodeJs = true;
  };
}
