let
  # A preset that installs a package and pins it to `pkgs.unstable`.
  mkUnstablePreset =
    name:
    {
      pkgs,
      lib,
      config,
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

in

{
  imports = [
    ./bat.nix
    ./bottom.nix
    ./claude-code.nix
    ./direnv.nix
    ./editor.nix
    ./emacs
    ./fd.nix
    ./firefox.nix
    ./fuzzel.nix
    ./gh.nix
    ./git.nix
    ./glow.nix
    ./nushell
    ./rofi.nix
    ./spotify-player.nix
    ./starship.nix
    ./swaylock.nix
    ./tmux
    ./waybar
    ./wezterm.nix
    ./zathura.nix

    (mkUnstablePreset "acpi")
    (mkUnstablePreset "bat")
    (mkUnstablePreset "bottom")
    (mkUnstablePreset "brightnessctl")
    (mkUnstablePreset "codex")
    (mkUnstablePreset "direnv")
    (mkUnstablePreset "dive")
    (mkUnstablePreset "doggo")
    (mkUnstablePreset "fd")
    (mkUnstablePreset "firefox")
    (mkUnstablePreset "fzf")
    (mkUnstablePreset "git")
    (mkUnstablePreset "glow")
    (mkUnstablePreset "grim")
    (mkUnstablePreset "jq")
    (mkUnstablePreset "miniserve")
    (mkUnstablePreset "nix-output-monitor")
    (mkUnstablePreset "nushell")
    (mkUnstablePreset "pamixer")
    (mkUnstablePreset "parted")
    (mkUnstablePreset "playerctl")
    (mkUnstablePreset "rofi")
    (mkUnstablePreset "slurp")
    (mkUnstablePreset "spotify-player")
    (mkUnstablePreset "starship")
    (mkUnstablePreset "swaylock")
    (mkUnstablePreset "termshark")
    (mkUnstablePreset "tmux")
    (mkUnstablePreset "viddy")
    (mkUnstablePreset "waybar")
    (mkUnstablePreset "wezterm")
    (mkUnstablePreset "wf-recorder")
    (mkUnstablePreset "whois")
    (mkUnstablePreset "wireplumber")
    (mkUnstablePreset "wl-clipboard")
    (mkUnstablePreset "zathura")
    (mkUnstablePreset "zoxide")
  ];
}
