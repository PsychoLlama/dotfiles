{
  imports = [
    (import ./_mk-unstable-preset.nix "wireplumber")
    ../extensions/programs/wireplumber.nix
  ];
}
