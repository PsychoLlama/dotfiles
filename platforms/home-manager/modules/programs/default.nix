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
    ./aider-chat.nix
    ./claude-code.nix
    ./emacs.nix
    ./glow.nix
    ./hyprland.nix
    ./nushell
    ./viu.nix
    ./wezterm.nix

    (makeProgramModule "acpi")
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
    (makeProgramModule "nix-output-monitor")
    (makeProgramModule "nmap")
    (makeProgramModule "onefetch")
    (makeProgramModule "pamixer")
    (makeProgramModule "parted")
    (makeProgramModule "playerctl")
    (makeProgramModule "pv")
    (makeProgramModule "radare2")
    (makeProgramModule "rage")
    (makeProgramModule "signal-desktop")
    (makeProgramModule "slurp")
    (makeProgramModule "sshfs")
    (makeProgramModule "termshark")
    (makeProgramModule "tokei")
    (makeProgramModule "tree-sitter")
    (makeProgramModule "viddy")
    (makeProgramModule "wf-recorder")
    (makeProgramModule "whois")
    (makeProgramModule "wireplumber")
    (makeProgramModule "wl-clipboard")
  ];
}
