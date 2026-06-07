{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.carapace;
in

{
  options.psychollama.presets.programs.carapace = {
    enable = lib.mkEnableOption "Multi-shell command argument completer";
  };

  config = lib.mkIf cfg.enable {
    programs.carapace = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.unstable.carapace;
    };

    # Fall back to bash completions for commands carapace has no native spec for.
    #
    # The bridge runs `bash --rcfile <carapace's file> -i`, which would normally
    # skip the user's bash setup. On NixOS it still works: `bashInteractive` is
    # built with `SYS_BASHRC=/etc/bashrc`, so the bridge's bash sources the
    # system rc regardless of `--rcfile`, and `programs.bash.completion.enable`
    # (on by default) wires that file to load the bash-completion framework.
    # The framework then resolves per-command completions from $XDG_DATA_DIRS,
    # which NixOS already populates. No bridge rcfile of our own is needed.
    home.sessionVariables.CARAPACE_BRIDGES = "bash";
  };
}
