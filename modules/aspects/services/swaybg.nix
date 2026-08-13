{
  exports.homeManager =
    { pkgs, ... }:

    {
      services.swaybg = {
        enable = true;
        package = pkgs.unstable.swaybg;
        image = "attic/images/wallpapers/current";
      };
    };
}
