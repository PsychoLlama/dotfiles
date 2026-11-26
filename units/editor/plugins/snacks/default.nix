{ rhizome, units, ... }:

let
  inherit (units.platform) editor;
in

rhizome.unit "editor.plugins.snacks" {
  unit = rhizome.component [
    (editor.module ({
      plugins.snacks-nvim.opts = {
        input.enabled = true;
        notifier.enabled = true;
      };
    }))
  ];
}
