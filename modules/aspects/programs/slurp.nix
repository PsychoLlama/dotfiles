{
  imports = [
    (import ./_mk-unstable-preset.nix "slurp")
    ../../platform/homeManager/programs/slurp.nix
  ];
}
