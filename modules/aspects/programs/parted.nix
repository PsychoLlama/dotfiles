{
  imports = [
    (import ./_mk-unstable-preset.nix "parted")
    ../../extensions/programs/parted.nix
  ];
}
