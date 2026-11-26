{
  config,
  inputs,
  units,
  ...
}:

let
  inherit (units.platform) editor host user;

  # The editor's units, evaluated with no machine around them.
  standalone = inputs.rhizome.lib.eval {
    levels = [
      host.level
      user.level
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
          modules = editor.lib.modules (standalone.collect editor.module);
        };

        inherit (pkgs.custom) chrome-devtools-mcp claude-code-bin codex-bin;
      };
    };
}
