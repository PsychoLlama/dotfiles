{ lib, config, ... }:

let
  cfg = config.psychollama.presets.programs.claude-code.plugins.rust-lsp;
in

{
  options.psychollama.presets.programs.claude-code.plugins.rust-lsp = {
    enable = lib.mkEnableOption "Rust LSP for Claude Code" // {
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.claude-code.localPlugins.rust-lsp = {
      description = "Rust Language Server for Claude Code.";

      lsp.servers.rust = {
        command = "rust-analyzer";
        extensionToLanguage.".rs" = "rust";
      };
    };
  };
}
