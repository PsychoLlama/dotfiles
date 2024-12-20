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
    with lib;
    {
      options.programs.${pkgName} = {
        enable = mkEnableOption "Whether to install ${pkgName}";
        package = mkPackageOption pkgs pkgName { };
      };

      config.home.packages = mkIf cfg.enable [ cfg.package ];
    };
in
{
  imports = [
    ./editor.nix
    ./glow.nix
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
    (makeProgramModule "wl-clipboard")
  ];
}
