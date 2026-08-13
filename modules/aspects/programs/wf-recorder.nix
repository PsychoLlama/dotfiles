{
  imports = [
    (import ./_mk-unstable-preset.nix "wf-recorder")
    ../../platform/homeManager/programs/wf-recorder.nix
  ];
}
