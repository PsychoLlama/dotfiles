{
  imports = [
    (import ./_mk-unstable-preset.nix "brightnessctl")
    ../../extensions/programs/brightnessctl.nix
  ];
}
