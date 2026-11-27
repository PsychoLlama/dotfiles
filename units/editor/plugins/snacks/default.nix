{ units, define, ... }:

let
  inherit (units) editor;
in

{
  unit = define.unit {
    provides = [
      (editor.plugin {
        name = "snacks-nvim";
        config = ./config.lua;
        opts = {
          input.enabled = true;
          notifier.enabled = true;
        };
      })
    ];
  };
}
