{ config, lib, pkgs, ... }:

with lib;

let cfg = config.services.ssh-agent;

in {
  options.services.ssh-agent = {
    enable = mkEnableOption "SSH agent";
    package = mkPackageOption pkgs "openssh" { };

    socket = mkOption {
      type = types.str;
      default = "ssh-agent.sock";
      description = ''
        The name of the socket to use. The socket will be created in
        <filename>/run/user/$UID/</filename> Linux, or
        <filename>/tmp/</filename> on macOS.
      '';
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && pkgs.stdenv.isLinux) {
      systemd.user.services.ssh-agent = {
        Install.WantedBy = [ "default.target" ];

        Unit = {
          Description = "SSH agent";
          Documentation = "man:ssh-agent(1)";
        };

        Service = {
          Type = "simple";
          Restart = "on-failure";
          ExecStartPre = "${pkgs.coreutils}/bin/rm -f %t/${cfg.socket}";
          ExecStart = "${cfg.package}/bin/ssh-agent -d -a %t/${cfg.socket}";
        };
      };
    })

    (mkIf (cfg.enable && pkgs.stdenv.isDarwin) {
      launchd.agents.ssh-agent = {
        enable = true;
        config = {
          KeepAlive = true;
          RunAtLoad = true;
          ProgramArguments = [
            (let
              socket = "/tmp/${cfg.socket}";
              scriptPath = pkgs.writers.writeBash "ssh-agent-service" ''
                ${pkgs.coreutils}/bin/rm -f ${escapeShellArg socket}
                ${cfg.package}/bin/ssh-agent -D -a ${escapeShellArg socket}
              '';
            in toString scriptPath)
          ];
        };
      };
    })
  ];
}
