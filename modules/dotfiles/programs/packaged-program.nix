name:

# A preset for a program home-manager doesn't model. There is no
# `programs.<name>` to point at, so this module *is* the program's module: it
# declares the package and installs it. The preset's `enable` and `package`
# are the whole surface — nothing downstream to mirror them onto.
#
# A plain `.nix` helper, not a module: discovery is per-file, so each program
# still needs its own `<name>.mod.nix` to stay individually enableable, and
# this keeps thirty-odd of those from being thirty-odd copies of the same
# four lines. Anything that grows real configuration should stop calling this
# and spell itself out.

{
  cfg,
  lib,
  pkgs,
  ...
}:

{
  options.package = lib.mkPackageOption pkgs.unstable name { };

  modules.home-manager.home.packages = [ cfg.package ];
}
