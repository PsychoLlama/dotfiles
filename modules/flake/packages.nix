{ config, ... }:

{
  perSystem =
    { pkgs, ... }:
    {
      packages = {
        editor = config.flake.lib.buildEditor {
          inherit pkgs;
          modules = [
            config.flake.modules.editor.configs
            { psychollama.profiles.full.enable = true; }
          ];
        };

        inherit (pkgs.custom) chrome-devtools-mcp claude-code-bin codex-bin;
      };
    };
}
