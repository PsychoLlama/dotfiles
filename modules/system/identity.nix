{ lib, ... }:

# Personal identity for the system's primary user. Required and shared across
# platforms; consumed by anything that needs to address or attribute the owner
# (login user, git, etc).
#
# Declared on the flake, like `agents` and `trusted-directories`. It was the last
# holdout in the `generic` class -- the only reason that class existed -- and the
# only one of the four with a real per-machine story: `username` keys
# `users.users.<name>`, so a flake option means one owner for every host built
# from a given flake. That is the actual shape of this config (one person, one
# fleet), and a downstream flake that needs two owners can set the option in each
# of its own host modules instead.

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
