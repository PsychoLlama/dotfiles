{
  imports = [
    (import ./_mk-unstable-preset.nix "brightnessctl")
    ../../platform/homeManager/programs/brightnessctl.nix
  ];
}
