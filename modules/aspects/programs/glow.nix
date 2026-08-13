{
  imports = [
    (import ./_mk-unstable-preset.nix "glow")
    ../../platform/homeManager/programs/glow.nix
  ];

  flake.modules.homeManager.default.programs.glow.settings = {
    local = true;
    pager = false;
  };
}
