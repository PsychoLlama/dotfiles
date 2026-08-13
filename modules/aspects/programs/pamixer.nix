{
  imports = [
    (import ./_mk-unstable-preset.nix "pamixer")
    ../../platform/homeManager/programs/pamixer.nix
  ];
}
