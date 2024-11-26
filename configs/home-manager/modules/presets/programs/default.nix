let
  # A preset that installs a package and pins it to `pkgs.unstable`.
  mkUnstablePreset =
    programName:
    {
      pkgs,
      lib,
      config,
      ...
    }:
    let
      cfg = config.programs.presets.${programName};

    in
    {
      options.programs.presets.${programName}.enable = lib.mkEnableOption "Install the latest version of ${programName}";

      config.programs.${programName} = lib.mkIf cfg.enable {
        enable = true;
        package = pkgs.unstable.${programName};
      };
    };

in
{
  imports = [
    ./alacritty.nix
    ./ast-grep.nix
    ./bat.nix
    ./bottom.nix
    ./direnv.nix
    ./dive.nix
    ./doggo.nix
    ./fd.nix
    ./firefox.nix
    ./fzf.nix
    ./git.nix
    ./glow.nix
    ./jq.nix
    ./llm.nix
    ./miniserve.nix
    ./neovim.nix
    ./nmap.nix
    ./nushell
    ./rofi.nix
    ./spotify-player.nix
    ./sshfs.nix
    ./starship.nix
    ./swaylock.nix
    ./termshark.nix
    ./tmux
    ./viddy.nix
    ./waybar
    ./wezterm.nix
    ./whois.nix
    ./zathura.nix
    ./zoxide.nix

    (mkUnstablePreset "aider-chat")
  ];
}
