{
  imports = [ (import ./_mk-unstable-preset.nix "glow") ];

  flake.modules.homeManager.default.programs.glow.settings = {
    local = true;
    pager = false;
  };
}
