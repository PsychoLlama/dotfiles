{
  imports = [
    (import ./_mk-unstable-preset.nix "nix-output-monitor")
    ../../extensions/programs/nix-output-monitor.nix
  ];
}
