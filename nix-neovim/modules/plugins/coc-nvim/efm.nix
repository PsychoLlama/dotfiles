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

    configFile = mkOption {
      type = types.path;
      description = "Generated configuration file";
      default = yamlFormat.generate "efm-config.yaml" cfg.settings;
      readOnly = true;
    };

    filetypes = mkOption {
      type = types.listOf types.str;
      description = "List of filetypes to enable EFM for";
      default = attrNames cfg.settings.languages;
      readOnly = true;
    };
  };

  config.plugins.coc-nvim.settings.languageserver.efm = mkIf cfg.enable {
    command = "${cfg.package}/bin/efm-langserver";
    filetypes = cfg.filetypes;
    args = [
      "-c"
      cfg.configFile
    ];
  };
}
