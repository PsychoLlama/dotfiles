{ rhizome, units, ... }:

let
  inherit (units) editor;
in

rhizome.unit "editor.plugins.snacks" {
  unit = rhizome.component [
    (editor.plugin {
      name = "snacks-nvim";
      config = ./config.lua;
      opts = {
        input.enabled = true;
        notifier.enabled = true;
      };
    })
  ];
}
