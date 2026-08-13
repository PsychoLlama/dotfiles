{
  imports = [
    (import ./_mk-unstable-preset.nix "dive")
    ../../platform/homeManager/programs/dive.nix
  ];
}
