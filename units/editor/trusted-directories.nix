{ rhizome, units, ... }:

let
  inherit (units.platform) editor user;
in

rhizome.component "editor.trusted-directories" {
  uses = {
    trusted-directories = user.trusted-directory;
  };

  config = { trusted-directories }: [
    (editor.module (_:

    {
      # An editor built without a host has no owner, so it trusts nothing.
      # `~` is expanded at runtime by the env framework.
      config.env.trusted = trusted-directories;
    }))
  ];
}
