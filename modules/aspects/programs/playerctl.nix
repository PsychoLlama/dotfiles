{
  imports = [
    (import ./_mk-unstable-preset.nix "playerctl")
    ../../platform/homeManager/programs/playerctl.nix
  ];
}
