{
  self,
  cfg,
  global,
  lib,
  ...
}:
{
  options = {
    greeting = lib.mkOption {
      type = lib.types.str;
      default = "hello";
    };

    # Reads another module through `global` — needs it mounted, not enabled.
    summary = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = "${cfg.greeting} on ${global."${self.theme}".palette.background}";
    };
  };

  # Enabling a peer is an explicit write to its handle.
  config."${self.services.beta}" = {
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
