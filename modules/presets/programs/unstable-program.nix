name:

# A preset whose whole job is installing a program from `pkgs.unstable`. A
# plain `.nix` helper, not a module: discovery is per-file, so each program
# still needs its own `<name>.mod.nix` to stay individually enableable, and
# this keeps twenty-odd of those from being twenty-odd copies of the same
# three lines. Anything that grows real configuration should stop calling
# this and spell itself out.

{ lib, pkgs, ... }:

{
  modules.home-manager.programs.${name} = {
    enable = lib.mkDefault true;
    package = lib.mkDefault pkgs.unstable.${name};
  };
}
