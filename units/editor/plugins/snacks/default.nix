{ rhizome, units, ... }:

let
  inherit (units.platform) editor;
in

rhizome.component "editor.plugins.snacks" [
  (editor.module ({
    plugins.snacks-nvim.opts = {
      input.enabled = true;
      notifier.enabled = true;
    };
  }))
]
