{
  imports = [
    (import ./_mk-unstable-preset.nix "wl-clipboard")
    ../../platform/homeManager/programs/wl-clipboard.nix
  ];
}
