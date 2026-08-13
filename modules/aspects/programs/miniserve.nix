{
  imports = [
    (import ./_mk-unstable-preset.nix "miniserve")
    ../../platform/homeManager/programs/miniserve.nix
  ];
}
