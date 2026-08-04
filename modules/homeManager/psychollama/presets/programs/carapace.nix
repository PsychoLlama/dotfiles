{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.carapace;

  # Carapace's bash bridge runs a bare `bash`, resolved via PATH. Inside nix
  # devshells that's stdenv's non-interactive bash, which lacks the `complete`
  # builtin, so the bridge silently produces nothing. Prepend an interactive
  # bash to carapace's own PATH so its bridge always finds a capable one.
  # Wrapping the default keeps the package overridable: overlaying
  # `unstable.carapace` flows through here, and `programs.carapace.package` can
  # still replace it outright.
  withBridgeBash =
    carapace:
    pkgs.symlinkJoin {
      name = "carapace-bridge-bash";
      paths = [ carapace ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/carapace \
          --prefix PATH : ${lib.makeBinPath [ pkgs.bashInteractive ]}
      '';
      meta.mainProgram = "carapace";
    };
in

{
  options.psychollama.presets.programs.carapace = {
    enable = lib.mkEnableOption "Multi-shell command argument completer";
  };

  config = lib.mkIf cfg.enable {
    programs.carapace = {
      enable = lib.mkDefault true;
      package = lib.mkDefault (withBridgeBash pkgs.unstable.carapace);
    };

    # Fall back to bash completions for commands carapace has no native spec for.
    # The bridge runs `bash --rcfile <its own file> -i`; although `--rcfile`
    # skips the user's `~/.bashrc`, NixOS builds `bashInteractive` with
    # `SYS_BASHRC=/etc/bashrc`, so the bridge's bash still sources the system rc.
    # `programs.bash.completion.enable` (on by default) wires that file to load
    # the bash-completion framework, which resolves per-command completions from
    # $XDG_DATA_DIRS. No custom bridge rcfile is needed.
    home.sessionVariables.CARAPACE_BRIDGES = "bash";
  };
}
