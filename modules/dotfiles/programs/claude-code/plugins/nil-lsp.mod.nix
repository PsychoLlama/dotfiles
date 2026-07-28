{
  cfg,
  lib,
  pkgs,
  ...
}:

let
  nil = lib.getExe cfg.package;
in

{
  options = {
    package = lib.mkPackageOption pkgs.unstable "nil" { };
  };

  modules.home-manager.programs.claude-code.localPlugins.nil-lsp = {
    description = "Nil (nix) Language Server for Claude Code.";

    lsp.servers.nil = {
      command = nil;
      extensionToLanguage.".nix" = "nix";
    };
  };
}
