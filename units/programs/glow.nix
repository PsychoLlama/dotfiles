{ units, define, ... }:

{
  unit = define.unit {
    config = [
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
  };
}
