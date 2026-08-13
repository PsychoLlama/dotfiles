{
  imports = [
    (import ./_mk-unstable-preset.nix "wireplumber")
    ../../platform/homeManager/programs/wireplumber.nix
  ];
}
