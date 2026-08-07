{
  imports = [
    (import ./_mk-unstable-preset.nix "grim")
    ../extensions/programs/grim.nix
  ];
}
