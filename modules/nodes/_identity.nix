{ lib, ... }:

# Personal identity for the machine's primary user, imported by a host that has
# an owner. Consumed by anything that needs to address or attribute them (login
# user, git, etc).

{
  options.identity = {
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
