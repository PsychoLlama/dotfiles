{ config, ... }:

{
  perSystem =
    { pkgs, ... }:
    {
      packages = {
        editor = config.flake.lib.buildEditor {
          inherit pkgs;
          modules = [
            { psychollama.profiles.full.enable = true; }
          ];
        };

        inherit (pkgs.custom) chrome-devtools-mcp claude-code-bin codex-bin;
      };
    };
}
