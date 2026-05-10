{ lib, ... }:

# Personal identity for the system's primary user. Required and shared across
# platforms; consumed by anything that needs to address or attribute the owner
# (login user, git, etc).

{
  options.psychollama.identity = {
    username = lib.mkOption {
      type = lib.types.str;
      description = "The primary username for the system.";
    };

    name = lib.mkOption {
      type = lib.types.str;
      description = "Real name of the system's owner.";
    };

    email = lib.mkOption {
      type = lib.types.str;
      description = "Email address of the system's owner.";
    };
  };
}
