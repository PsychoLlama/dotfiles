{ lib, config, ... }:

#############################################
# Add richer DSLs for configuring Hyprland. #
#############################################

let
  inherit (lib) types;
  cfg = config.wayland.windowManager.hyprland;
in

{
  options.wayland.windowManager.hyprland = {
    bindings = lib.mkOption {
      description = "Structured options over `settings.bind`";
      default = [ ];

      type = types.listOf (
        types.submodule {
          options.dispatcher = lib.mkOption {
            type = types.str;
            default = "exec";
            description = ''
              Name of the dispatcher to execute.
              See: https://wiki.hyprland.org/Configuring/Dispatchers/
            '';
          };

          options.modifiers = lib.mkOption {
            type = types.listOf types.str;
            description = "List of modifiers to use with the key binding";
            default = [ ];
          };

          options.key = lib.mkOption {
            type = types.str;
            description = "Key to bind";
          };

          options.action = lib.mkOption {
            type = types.either types.str types.package;
            default = "";
            description = ''
              Action to take when the key binding is triggered. May be omitted
              for some dispatchers.
            '';
          };
        }
      );
    };
  };

  config.wayland.windowManager.hyprland = lib.mkIf cfg.enable {
    # Hyprland expects a 4-tuple of (modifiers, key, dispatcher, action).
    settings.bind = lib.forEach cfg.bindings (
      binding:
      lib.concatStringsSep ", " [
        "${lib.concatStringsSep " " binding.modifiers}"
        binding.key
        binding.dispatcher
        binding.action
      ]
    );
  };
}
