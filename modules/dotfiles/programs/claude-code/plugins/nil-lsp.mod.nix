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

  config.programs.claude-code.marketplace.plugins.nil-lsp = {
    description = "Nil (nix) Language Server for Claude Code.";

    lsp.servers.nil = {
      command = nil;
      extensionToLanguage.".nix" = "nix";
    };
  };
}
