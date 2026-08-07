name:

# A preset that installs a program and pins it to `pkgs.unstable`. Programs
# needing more than a package import this alongside their own config.

{
  flake.modules.homeManager.default =
    { lib, pkgs, ... }:

    {
      programs.${name} = {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.unstable.${name};
      };
    };
}
