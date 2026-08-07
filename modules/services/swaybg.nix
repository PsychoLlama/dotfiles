{
  imports = [ ../extensions/services/swaybg.nix ];

  flake.modules.homeManager.default =
    { pkgs, ... }:

    {
      services.swaybg = {
        enable = true;
        package = pkgs.unstable.swaybg;
        image = "attic/images/wallpapers/current";
      };
    };
}
