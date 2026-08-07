{ config, ... }:

{
  perSystem =
    { pkgs, ... }:
    {
      packages = {
        editor = config.flake.lib.buildEditor { inherit pkgs; };

        inherit (pkgs.custom) chrome-devtools-mcp claude-code-bin codex-bin;
      };
    };
}
