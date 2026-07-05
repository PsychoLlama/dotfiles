{ lib, config, ... }:

let
  cfg = config.psychollama.presets.programs.claude-code.plugins.lua-lsp;
in

{
  options.psychollama.presets.programs.claude-code.plugins.lua-lsp = {
    enable = lib.mkEnableOption "Lua LSP for Claude Code" // {
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.claude-code.localPlugins.lua-lsp = {
      description = "Lua Language Server for Claude Code.";

      lsp.servers.lua = {
        command = "lua-language-server";
        extensionToLanguage.".lua" = "lua";
      };
    };
  };
}
