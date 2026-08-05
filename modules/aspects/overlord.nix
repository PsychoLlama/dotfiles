{
  den,
  dotfiles,
  ...
}:

# Aspect for the `overlord` user. Keys are module classes.

{
  den.aspects.overlord = {
    includes = [
      den.batteries.primary-user
      dotfiles.home-lab-admin
      dotfiles.services.restic
      dotfiles.services.syncthing
    ];

    # Routed to `users.users.overlord`.
    user =
      { user, osConfig, ... }:

      {
        description = user.identity.name;
        shell = osConfig.home-manager.users.overlord.programs.nushell.package;
        extraGroups = [
          "dialout"
          "podman"
        ];
      };

    # Routed to the host. Keeps the login shell available system-wide.
    os =
      { config, ... }:

      {
        environment.shells = [ config.users.users.overlord.shell ];
      };

    homeManager =
      { config, pkgs, ... }:

      {
        home.stateVersion = "22.05";
        home.packages = [ pkgs.man-pages ];

        wayland.windowManager.sway.config.output = {
          # Built in display.
          "eDP-1".position = "1440 2360";

          # External monitor.
          "LG Electronics LG ULTRAWIDE 404NTLEDA584" = {
            # Most of my time is spent reading. Using an ultrawide in portrait
            # looks super weird but wow is it a game changer.
            transform = "90";
            position = "0 0";
          };
        };

        # Where the flake lives on disk, used by `nh os` / `nh home`.
        programs.nh.flake = "${config.home.homeDirectory}/projects/psychollama/dotfiles";

        psychollama.profiles = {
          full.enable = true;
          linux-desktop.enable = true;
        };
      };
  };
}
