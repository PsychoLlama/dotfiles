{
  imports = [
    (import ./_mk-unstable-preset.nix "glow")
    ../../extensions/programs/glow.nix
  ];

  flake.modules.homeManager.default.programs.glow.settings = {
    local = true;
    pager = false;
  };
}
