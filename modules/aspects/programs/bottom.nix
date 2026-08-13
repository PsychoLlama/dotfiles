{
  imports = [ (import ./_mk-unstable-preset.nix "bottom") ];

  exports.homeManager.programs.bottom.settings.flags.temperature_type = "f";
}
