{
  imports = [
    (import ./_mk-unstable-preset.nix "viddy")
    ../extensions/programs/viddy.nix
  ];
}
