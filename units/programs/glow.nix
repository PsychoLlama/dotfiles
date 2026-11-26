{ rhizome, units, ... }:

rhizome.unit "glow" {
  unit = rhizome.component [
    (units.platform.home-manager.module (
      { pkgs, ... }:

      {
        home.packages = [ pkgs.unstable.glow ];

        xdg.configFile."glow/glow.yml".source = (pkgs.formats.yaml { }).generate "glow-config" {
          local = true;
          pager = false;
        };
      }
    ))
  ];
}
