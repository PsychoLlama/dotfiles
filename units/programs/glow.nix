{ units, module, ... }:

{
  unit = module.component {
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
