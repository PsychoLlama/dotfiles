{
  imports = [
    (import ./_mk-unstable-preset.nix "wf-recorder")
    ../../extensions/programs/wf-recorder.nix
  ];
}
