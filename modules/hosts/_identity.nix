{ lib, ... }:

# Not a flake module. This extends `rhizome.hosts.<name>`, imported by a host
# that has an owner, and read by aspects through the `host` module argument.
# Underscore-prefixed so the sweep skips it.
#
# Per-host rather than per-flake because `username` keys `users.users.<name>`:
# a flake option would mean one owner for every machine built from a given
# flake. Not every host has one -- a headless box may never import this.

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
