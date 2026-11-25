{ rhizome, units, ... }:

let
  inherit (units.platform) editor;
in

rhizome.component "editor.trusted-directories" [
  (editor.module (
    { host, ... }:

    {
      # An editor built without a host has no owner, so it trusts nothing.
      # `~` is expanded at runtime by the env framework.
      config.env.trusted = host.trusted-directories or [ ];
    }
  ))
]
