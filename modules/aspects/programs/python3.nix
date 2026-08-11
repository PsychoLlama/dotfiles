{
  imports = [
    (import ./_mk-unstable-preset.nix "python3")
    ../../extensions/programs/python3.nix
  ];
}
