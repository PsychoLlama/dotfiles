{
  imports = [ (import ./_mk-unstable-preset.nix "bat") ];

  flake.modules.homeManager.default =
    { config, lib, ... }:

    let
      cfg = config.psychollama.presets.programs.bat;
    in

    {
      config = lib.mkIf cfg.enable {
        home.shellAliases.cat = "bat";

        programs.bat = {
          config = {
            theme = "TwoDark";
            style = "changes";
          };
        };
      };
    };
}
