{ lib, ... }:

{
  options.psychollama.settings = {
    username = lib.mkOption {
      type = lib.types.str;
      description = ''
        The primary username for the system.
      '';
    };
  };
}
