{ config, lib, ... }:

with lib;

let cfg = config.plugins.markdown-preview-nvim;

in {
  options.plugins.markdown-preview-nvim.browser = mkOption {
    type = types.nullOr types.str;
    default = null;
    description = "Browser to use for markdown preview";
  };

  config.plugins.markdown-preview-nvim.extraConfig =
    mkIf (cfg.browser != null) ''
      let g:mkdp_browser = "${cfg.browser}"
    '';
}
