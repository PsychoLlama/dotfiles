{
  config,
  inputs,
  units,
  ...
}:

let
  inherit (units.platform) editor level;

  # The editor's units, evaluated with no machine around them.
  standalone = inputs.rhizome.lib.eval {
    levels = [
      level.host
      level.user
    ];
  } units.editor.profiles.full.unit;
in

{
  perSystem =
    { pkgs, ... }:
    {
      packages = {
        editor = config.flake.lib.editor {
          inherit pkgs;
          modules = editor.modules (standalone.collect editor.module);
        };

        inherit (pkgs.custom) chrome-devtools-mcp claude-code-bin codex-bin;
      };
    };
}
