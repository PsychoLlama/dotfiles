{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.lsp.efm;
  yaml = pkgs.formats.yaml { };
in
{
  options.lsp.efm = {
    enable = mkEnableOption "Enable the EFM language server";
    package = mkPackageOption pkgs "efm-langserver" { };
    settings = mkOption {
      type = yaml.type;
      description = "EFM language server configuration";
      default = { };
    };

    filetypes = mkOption {
      type = types.listOf types.str;
      description = "List of filetypes to enable EFM for";
      default = attrNames cfg.settings.languages;
      readOnly = true;
    };
  };

  config.lsp.servers.efm-langserver = mkIf cfg.enable {
    server = "${cfg.package}/bin/efm-langserver";
    filetypes = cfg.filetypes;
    root.patterns = [ ".git/" ];
    args = [
      "-c"
      (yaml.generate "efm-config.yaml" cfg.settings)
    ];
  };
}
