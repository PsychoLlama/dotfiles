{ units, module, ... }:

let
  inherit (units) editor;
in

{
  unit = module.component "unit" {
    config = [
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
