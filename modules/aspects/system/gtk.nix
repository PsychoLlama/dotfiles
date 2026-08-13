{
  exports.homeManager =
    { pkgs, ... }:

    {
      gtk = {
        enable = true;
        colorScheme = "dark";
      };

      home.pointerCursor = {
        name = "Adwaita";
        package = pkgs.unstable.adwaita-icon-theme;
        size = 24;
        gtk.enable = true;
      };

      dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
    };
}
