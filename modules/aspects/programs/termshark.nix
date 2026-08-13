{
  imports = [
    (import ./_mk-unstable-preset.nix "termshark")
    ../../platform/homeManager/programs/termshark.nix
  ];
}
