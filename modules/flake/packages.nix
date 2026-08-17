{ config, ... }:

{
  perSystem =
    { pkgs, ... }:
    {
      packages = {
        editor = config.flake.lib.editor {
          inherit pkgs;
          modules = [ config.flake.rhizomeModules."editor/profiles/full".editor ];
        };

        inherit (pkgs.custom) chrome-devtools-mcp claude-code-bin codex-bin;
      };
    };
}
