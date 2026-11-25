# What `ava` is made of. The hardware and the machine's own settings are
# still in `modules/nodes/ava`, which imports what this produces.
{ units, ... }:

let
  inherit (units.platform) home-manager nixos;
  inherit (units) profiles;
in

{ username }:

nixos.host [
  (home-manager.user { name = username; } [
    profiles.substrate
    profiles.full
    profiles.home-lab-admin
    profiles.linux-desktop
  ])
]
