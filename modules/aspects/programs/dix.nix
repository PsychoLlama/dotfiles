{
  imports = [
    (import ./_mk-unstable-preset.nix "dix")
    ../../platform/homeManager/programs/dix.nix
  ];
}
