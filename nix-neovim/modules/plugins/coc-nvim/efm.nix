{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.plugins.coc-nvim.efm;
  yamlFormat = pkgs.formats.yaml { };
in
{
  options.plugins.coc-nvim.efm = {
    enable = mkEnableOption "Enable the EFM language server";
    package = mkPackageOption pkgs "efm-langserver" { };
    settings = mkOption {
      type = yamlFormat.type;
      description = "EFM language server configuration";
      default = { };
    };
  };

  config.plugins.coc-nvim.settings.languageserver.efm = mkIf cfg.enable {
    command = "${cfg.package}/bin/efm-langserver";
    args = [
      "-c"
      (yamlFormat.generate "efm-config.yaml" cfg.settings)
    ];
    filetypes = attrNames cfg.settings.languages;
  };
}
