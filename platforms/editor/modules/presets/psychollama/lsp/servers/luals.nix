{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.presets.lsp.servers.luals;
in

{
  options.presets.lsp.servers.luals = {
    enable = lib.mkEnableOption "Use JSON language server";
    package = lib.mkPackageOption pkgs.unstable "lua-language-server" { };
  };

  config.lsp.servers.luals = lib.mkIf cfg.enable {
    server = "${cfg.package}/bin/lua-language-server";
    filetypes = [ "lua" ];
    root.patterns = [
      ".git/"
      ".luarc.json"
    ];

    # Reference: https://luals.github.io/wiki/settings/
    settings.Lua = {
      # Using stylua instead.
      format.enable = false;

      # Don't try to dynamically manage library type defs.
      workspace.checkThirdParty = false;
      addonManager.enable = false;
    };
  };
}
