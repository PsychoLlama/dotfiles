{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code.plugins.nil-lsp;
  nil = lib.getExe cfg.package;
in

{
  options.psychollama.presets.programs.claude-code.plugins.nil-lsp = {
    package = lib.mkPackageOption pkgs.unstable "nil" { };
    enable = lib.mkEnableOption "Nil (nix) LSP for Claude Code" // {
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.claude-code.localPlugins.nil-lsp = {
      description = "Nil (nix) Language Server for Claude Code.";

      lsp.servers.nil = {
        command = nil;
        extensionToLanguage.".nix" = "nix";
      };
    };
  };
}
