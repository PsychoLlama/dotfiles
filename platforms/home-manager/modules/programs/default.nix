let
  # Define a simple `programs.<name>` module that installs and configures an
  # arbitrary package.
  makeProgramModule =
    pkgName:
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      cfg = config.programs.${pkgName};
    in

    {
      options.programs.${pkgName} = {
        enable = lib.mkEnableOption "Whether to install ${pkgName}";
        package = lib.mkPackageOption pkgs pkgName { };
      };

      config.home.packages = lib.mkIf cfg.enable [ cfg.package ];
    };
in
{
  imports = [
    (makeProgramModule "acpi")
    (makeProgramModule "binutils")
    (makeProgramModule "brightnessctl")
    (makeProgramModule "codex")
    (makeProgramModule "dive")
    (makeProgramModule "dogdns")
    (makeProgramModule "doggo")
    (makeProgramModule "du-dust")
    (makeProgramModule "duf")
    (makeProgramModule "grim")
    (makeProgramModule "hexyl")
    (makeProgramModule "miniserve")
    (makeProgramModule "nix-output-monitor")
    (makeProgramModule "onefetch")
    (makeProgramModule "pamixer")
    (makeProgramModule "parted")
    (makeProgramModule "playerctl")
    (makeProgramModule "pv")
    (makeProgramModule "radare2")
    (makeProgramModule "rage")
    (makeProgramModule "signal-desktop")
    (makeProgramModule "slurp")
    (makeProgramModule "termshark")
    (makeProgramModule "tokei")
    (makeProgramModule "viddy")
    (makeProgramModule "wf-recorder")
    (makeProgramModule "whois")
    (makeProgramModule "wireplumber")
    (makeProgramModule "wl-clipboard")
  ];
}
