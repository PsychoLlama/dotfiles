{
  imports = [
    (import ./_mk-unstable-preset.nix "glow")
  ];

  exports.homeManager.programs.glow.settings = {
    local = true;
    pager = false;
  };
}
