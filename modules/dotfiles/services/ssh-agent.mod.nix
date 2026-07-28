{ pkgs, ... }:

{
  modules.home-manager.services.ssh-agent = {
    enable = true;
    package = pkgs.unstable.openssh;
  };
}
