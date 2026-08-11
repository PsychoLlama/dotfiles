{
  imports = [
    (import ./_mk-unstable-preset.nix "doggo")
    ../../extensions/programs/doggo.nix
  ];
}
