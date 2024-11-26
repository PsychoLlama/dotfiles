{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.tmux;
  tmux = config.programs.tmux.package;
in
{
  options.presets.programs.tmux.enable = mkEnableOption "Use an opinionated tmux configuration";

  config = mkMerge [
    (mkIf cfg.enable {
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
          ${builtins.readFile ./tmux.conf}

          bind-key C-s display-popup -E ${pkgs.writers.writeBash "tmux-jump" ''
            sessions="$(${tmux}/bin/tmux list-sessions -F "#{session_name}")"
            session_name="$(echo -e "$sessions" | ${pkgs.fzf}/bin/fzf)"

            if [[ -n "$session_name" ]]; then
              ${tmux}/bin/tmux switch-client -t "$session_name"
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
    })

    (mkIf (cfg.enable && pkgs.stdenv.isLinux) {
      systemd.user.services.tmux-server = {
        Install.WantedBy = [ "default.target" ];

        Unit = {
          Description = "tmux server";
          Documentation = "man:tmux(1)";
        };

        Service = {
          Type = "oneshot";
          ExecStart = "${tmux}/bin/tmux start-server";
          RemainAfterExit = true;
        };
      };
    })

    # Running tmux through launchctl on macOS means I don't need to grant app
    # permissions to every terminal emulator. Calls route through the owning
    # process instead.
    (mkIf (cfg.enable && pkgs.stdenv.isDarwin) {
      launchd.agents.tmux-server = {
        enable = true;
        config = {
          RunAtLoad = true;
          ProgramArguments = [ "${tmux}/bin/tmux start-server" ];
        };
      };
    })
  ];
}
