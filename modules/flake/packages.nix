{ config, ... }:

{
  perSystem =
    { pkgs, ... }:
    {
      packages = {
        editor = config.flake.lib.editor {
          inherit pkgs;
          modules = [ config.flake.modules.editor."editor/profiles/full" ];
        };

        inherit (pkgs.custom) chrome-devtools-mcp claude-code-bin codex-bin;
      };
    };
}
