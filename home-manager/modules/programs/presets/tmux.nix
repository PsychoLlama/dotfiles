{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.presets.tmux;
in
{
  options.programs.presets.tmux.enable = mkEnableOption "Use an opinionated tmux configuration";

  config = mkIf cfg.enable {
    home.shellAliases.t = "tmux";

    programs.tmux = {
      enable = true;
      package = pkgs.unstable.tmux;
      customPaneNavigationAndResize = true;
      escapeTime = 0;
      historyLimit = 100000;
      keyMode = "vi";
      shell = "${config.programs.nushell.package}/bin/nu";
      extraConfig = ''
        ${builtins.readFile ../../../../config/tmux.conf}
        bind-key j display-popup -E ${pkgs.writeScript "tmux-jump" ''
          sessions="$(tmux list-sessions -F "#{session_name}")"
          session_name="$(echo -e "$sessions" | ${pkgs.fzf}/bin/fzf)"

          if [[ -n "$session_name" ]]; then
            tmux switch-client -t "$session_name"
          fi
        ''}
      '';
    };

    # The default session variable attempts POSIX interpolation, which
    # obviously doesn't work in Nushell.
    programs.nushell.extraEnv =
      if pkgs.stdenv.isDarwin then
        ''
          $env.TMUX_TMPDIR = "/tmp"
        ''
      else
        ''
          $env.TMUX_TMPDIR = $env.XDG_RUNTIME_DIR
        '';
  };
}
