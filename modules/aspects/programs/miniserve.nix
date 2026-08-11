{
  imports = [
    (import ./_mk-unstable-preset.nix "miniserve")
    ../../extensions/programs/miniserve.nix
  ];
}
