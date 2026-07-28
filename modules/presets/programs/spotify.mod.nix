{ pkgs, ... }:

# The desktop client has no home-manager module, so it ships as a bare package.
{
  modules.home-manager.home.packages = [ pkgs.unstable.spotify ];
}
