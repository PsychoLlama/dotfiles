{
  imports = [
    (import ./_mk-unstable-preset.nix "parted")
    ../../platform/homeManager/programs/parted.nix
  ];
}
