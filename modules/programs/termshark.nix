{
  imports = [
    (import ./_mk-unstable-preset.nix "termshark")
    ../extensions/programs/termshark.nix
  ];
}
