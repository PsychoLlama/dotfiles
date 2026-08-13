{
  imports = [
    (import ./_mk-unstable-preset.nix "glow")
    ../../platform/homeManager/programs/glow.nix
  ];

  exports.homeManager.programs.glow.settings = {
    local = true;
    pager = false;
  };
}
