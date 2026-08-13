{
  imports = [
    (import ./_mk-unstable-preset.nix "doggo")
    ../../platform/homeManager/programs/doggo.nix
  ];
}
