{ lib, config, ... }:

let
  cfg = config.plugins.alternaut-vim;
  lua = lib.generators.toLua { };
in

{
  options.plugins.alternaut-vim.patterns = lib.mkOption {
    type = lib.types.anything;
    description = "File naming conventions and patterns";
    default = { };
  };

  config.plugins.alternaut-vim.extraConfig = lib.mkIf (cfg.patterns != { }) ''
    vim.g['alternaut#conventions'] = ${lua cfg.patterns}
  '';
}
