{
  flake.modules.homeManager.default =
    {
      lib,
      config,
      pkgs,
      ...
    }:

    let
      cfg = config.psychollama.presets.programs.claude-code.plugins.nil-lsp;
    in

    {
      options.psychollama.presets.programs.claude-code.plugins.nil-lsp = {
        package = lib.mkPackageOption pkgs.unstable "nil" { };
      };

      config.programs.claude-code.plugins.nil-lsp = {
        description = "Nil (nix) Language Server for Claude Code.";

        lsp.servers.nil = {
          command = lib.getExe cfg.package;
          extensionToLanguage.".nix" = "nix";
        };
      };
    };
}
