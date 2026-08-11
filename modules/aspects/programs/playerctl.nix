{
  imports = [
    (import ./_mk-unstable-preset.nix "playerctl")
    ../../extensions/programs/playerctl.nix
  ];
}
