{
  self,
  cfg,
  lib,
  ...
}:
{
  options = {
    greeting = lib.mkOption {
      type = lib.types.str;
      default = "hello";
    };

    # Reads a sibling module through `self` — needs it mounted, not enabled.
    summary = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = "${cfg.greeting} on ${self.theme.palette.background}";
    };

    # `self` covers the plugin's root options too, not just its modules.
    rootView = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = self.themeName;
    };
  };

  # Enabling a peer is an explicit write, addressed from the plugin handle.
  config."${self}".services.beta = {
    enable = true;
    message = "${cfg.greeting} from alpha";
  };

  platforms = {
    # Root-class fragment: merges into the live fixpoint as config.
    test.hostSetting = "alpha was here";

    # Foreign-class fragment: deferred, takes the widget eval's own args
    # while closing over meta scope.
    widget =
      { prefix, ... }:
      {
        label = "${prefix}: ${cfg.greeting}";
      };
  };
}
