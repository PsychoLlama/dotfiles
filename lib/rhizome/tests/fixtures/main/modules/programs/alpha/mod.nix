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

    # `self` covers the plugin's own options too, not just its modules.
    pluginView = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = self.themeName;
    };
  };

  # `config` is this plugin's namespace: the mount point is implied, so
  # enabling another plugin's module is a plain write.
  config.services.beta = {
    enable = true;
    message = "${cfg.greeting} from alpha";
  };

  modules = {
    # Root-class fragment: merges into the live fixpoint as config.
    test.hostSetting = "alpha was here";

    # Foreign-class fragment: deferred, takes the widget eval's own args
    # while closing over rhizome scope.
    widget =
      { prefix, ... }:
      {
        label = "${prefix}: ${cfg.greeting}";
      };
  };
}
