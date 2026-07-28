name:

# A preset whose whole job is pointing home-manager's own `programs.<name>`
# module at `pkgs.unstable`. For a program home-manager doesn't model, reach
# for `packaged-program.nix` instead — it declares the package option here
# rather than borrowing one.
#
# A plain `.nix` helper, not a module: discovery is per-file, so each program
# still needs its own `<name>.mod.nix` to stay individually enableable, and
# this keeps a pile of those from being a pile of copies of the same three
# lines. Anything that grows real configuration should stop calling this and
# spell itself out.

{ lib, pkgs, ... }:

{
  modules.home-manager.programs.${name} = {
    enable = lib.mkDefault true;
    package = lib.mkDefault pkgs.unstable.${name};
  };
}
