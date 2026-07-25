{ pkgs, ... }:

{
  platforms.home-manager.services.ssh-agent = {
    enable = true;
    package = pkgs.unstable.openssh;
  };
}
