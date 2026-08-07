{
  imports = [ ../system/theme.nix ];

  flake.modules.homeManager.default =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      inherit (config.theme) palette;
      semi-black = "${palette.normal.black}d9"; # alpha(0.85)
    in

    {
      programs.swaylock = {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.unstable.swaylock;

        settings = {
          image = "attic/images/wallpapers/current";
          daemonize = true;
          show-failed-attempts = true;
          ignore-empty-password = true;

          color = palette.normal.black;
          line-color = palette.normal.black;

          ring-color = semi-black;
          inside-color = semi-black;
          text-color = palette.normal.red;
          key-hl-color = palette.normal.green;
          bs-hl-color = palette.normal.red;

          ring-clear-color = palette.bright.white;
          inside-clear-color = semi-black;
          text-clear-color = palette.bright.white;

          ring-ver-color = palette.normal.yellow;
          inside-ver-color = semi-black;
          text-ver-color = palette.normal.yellow;

          ring-wrong-color = palette.normal.red;
          inside-wrong-color = semi-black;
          text-wrong-color = palette.normal.red;
        };
      };
    };
}
