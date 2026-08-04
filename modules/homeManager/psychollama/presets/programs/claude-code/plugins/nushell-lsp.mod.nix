{ lib, config, ... }:

let
  cfg = config.psychollama.presets.programs.claude-code.plugins.nushell-lsp;
in

{
  options.psychollama.presets.programs.claude-code.plugins.nushell-lsp = {
    enable = lib.mkEnableOption "Nushell LSP for Claude Code" // {
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.claude-code.localPlugins.nushell-lsp = {
      description = "Nushell Language Server for Claude Code.";

      lsp.servers.nushell = {
        command = "nu";
        args = [ "--lsp" ];
        extensionToLanguage.".nu" = "nu";
      };
    };
  };
}
