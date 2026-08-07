name:

# A preset that installs a program and pins it to `pkgs.unstable`. Programs
# needing more than enable + package import this alongside their own config.

{
  flake.modules.homeManager.default =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      cfg = config.psychollama.presets.programs.${name};
    in

    {
      options.psychollama.presets.programs.${name}.enable =
        lib.mkEnableOption "Install the latest version of ${name}";

      config.programs.${name} = lib.mkIf cfg.enable {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.unstable.${name};
      };
    };
}
