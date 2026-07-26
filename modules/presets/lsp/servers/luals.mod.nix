{
  cfg,
  lib,
  pkgs,
  ...
}:

{
  options.package = lib.mkPackageOption pkgs.unstable "lua-language-server" { };

  platforms.editor.lsp.servers.luals = {
    cmd = [ "${cfg.package}/bin/lua-language-server" ];
    filetypes = [ "lua" ];
    root_markers = [
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
