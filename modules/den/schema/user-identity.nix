{ lib, ... }:

# Who the user is, as entity data. Grouped under `identity` because Den already
# uses `user.name` for the entity's own name.

{
  den.schema.user.options.identity = {
    name = lib.mkOption {
      type = lib.types.str;
      description = "Real name of the person behind the account.";
    };

    email = lib.mkOption {
      type = lib.types.str;
      description = "Email address of the person behind the account.";
    };
  };
}
