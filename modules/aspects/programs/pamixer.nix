{
  imports = [
    (import ./_mk-unstable-preset.nix "pamixer")
    ../../extensions/programs/pamixer.nix
  ];
}
