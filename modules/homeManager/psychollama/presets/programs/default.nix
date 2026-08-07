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
    (mkUnstablePreset "acpi")
    (mkUnstablePreset "brightnessctl")
    (mkUnstablePreset "dix")
    (mkUnstablePreset "dive")
    (mkUnstablePreset "doggo")
    (mkUnstablePreset "fzf")
    (mkUnstablePreset "grim")
    (mkUnstablePreset "jq")
    (mkUnstablePreset "miniserve")
    (mkUnstablePreset "nix-output-monitor")
    (mkUnstablePreset "pamixer")
    (mkUnstablePreset "parted")
    (mkUnstablePreset "playerctl")
    (mkUnstablePreset "python3")
    (mkUnstablePreset "slurp")
    (mkUnstablePreset "termshark")
    (mkUnstablePreset "viddy")
    (mkUnstablePreset "wf-recorder")
    (mkUnstablePreset "whois")
    (mkUnstablePreset "wireplumber")
    (mkUnstablePreset "wl-clipboard")
    (mkUnstablePreset "zoxide")
  ];
}
