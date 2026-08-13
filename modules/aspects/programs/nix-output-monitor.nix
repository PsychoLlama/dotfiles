{
  imports = [
    (import ./_mk-unstable-preset.nix "nix-output-monitor")
    ../../platform/homeManager/programs/nix-output-monitor.nix
  ];
}
