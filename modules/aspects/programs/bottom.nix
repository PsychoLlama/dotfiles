{
  imports = [ (import ./_mk-unstable-preset.nix "bottom") ];

  flake.modules.homeManager.default.programs.bottom.settings.flags.temperature_type = "f";
}
