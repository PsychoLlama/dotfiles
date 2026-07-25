{ lib, ... }:

# Personal identity for the system's primary user. Required and shared across
# platforms; consumed by anything that needs to address or attribute the owner
# (login user, git, etc).

{
  options = {
    # Data, not an effect. See `theme` for the reasoning.
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to publish the owner's identity.";
    };

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
