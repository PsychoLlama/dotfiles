{
  imports = [
    (import ./_mk-unstable-preset.nix "dive")
    ../extensions/programs/dive.nix
  ];
}
