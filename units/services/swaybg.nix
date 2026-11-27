# The wallpaper.
{ units, define, ... }:

{
  unit = define.unit {
    config = [
      (units.platform.home-manager.module (
        { pkgs, ... }:

        {
          systemd.user.services.swaybg = {
            Install.WantedBy = [ "sway-session.target" ];

            Unit = {
              Description = "Sway wallpaper and background service";
              PartOf = [ "sway-session.target" ];
              Documentation = "man:swaybg(1)";
            };

            Service = {
              Type = "simple";

              # The image is relative to `$HOME`.
              ExecStart = "${pkgs.unstable.swaybg}/bin/swaybg --image attic/images/wallpapers/current --mode fill --output *";
            };
          };
        }
      ))
    ];
  };
}
