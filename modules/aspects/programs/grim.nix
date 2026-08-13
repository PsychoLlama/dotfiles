{
  imports = [
    (import ./_mk-unstable-preset.nix "grim")
    ../../platform/homeManager/programs/grim.nix
  ];
}
