{
  imports = [
    (import ./_mk-unstable-preset.nix "viddy")
    ../../platform/homeManager/programs/viddy.nix
  ];
}
