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
    ./glow.nix
    ./hyprland.nix
    ./nushell

    (makeProgramModule "acpi")
    (makeProgramModule "aider-chat")
    (makeProgramModule "ast-grep")
    (makeProgramModule "binutils")
    (makeProgramModule "brightnessctl")
    (makeProgramModule "dive")
    (makeProgramModule "dogdns")
    (makeProgramModule "doggo")
    (makeProgramModule "du-dust")
    (makeProgramModule "duf")
    (makeProgramModule "grim")
    (makeProgramModule "hexyl")
    (makeProgramModule "ipfs")
    (makeProgramModule "llm")
    (makeProgramModule "miniserve")
    (makeProgramModule "mods")
    (makeProgramModule "nmap")
    (makeProgramModule "onefetch")
    (makeProgramModule "pamixer")
    (makeProgramModule "parted")
    (makeProgramModule "playerctl")
    (makeProgramModule "pv")
    (makeProgramModule "radare2")
    (makeProgramModule "rage")
    (makeProgramModule "slurp")
    (makeProgramModule "sshfs")
    (makeProgramModule "termshark")
    (makeProgramModule "tokei")
    (makeProgramModule "viddy")
    (makeProgramModule "viu")
    (makeProgramModule "wf-recorder")
    (makeProgramModule "whois")
    (makeProgramModule "wireplumber")
    (makeProgramModule "wl-clipboard")
  ];
}
