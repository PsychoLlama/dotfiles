{ lib, ... }:

# The system's primary user. Real name and email moved to the user entity
# (`den.schema.user.options.identity`); this remains for NixOS presets that
# still resolve the primary account by name.

{
  options.psychollama.identity = {
    username = lib.mkOption {
      type = lib.types.str;
      description = "The primary username for the system.";
    };
  };
}
