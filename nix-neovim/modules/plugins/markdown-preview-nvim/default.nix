{ config, lib, ... }:

with lib;

let
  cfg = config.plugins.markdown-preview-nvim;
  lua = lib.generators.toLua { };
in

{
  options.plugins.markdown-preview-nvim.browser = mkOption {
    type = types.nullOr types.str;
    default = null;
    description = "Browser to use for markdown preview";
  };

  config.plugins.markdown-preview-nvim.extraConfig = mkIf (cfg.browser != null) ''
    vim.g.mkdp_browser = ${lua cfg.browser};
  '';
}
