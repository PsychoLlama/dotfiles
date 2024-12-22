{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.auth-agent;
in

{
  # NOTE: This service used to be called `ssh-agent` but HM 23.11 added
  # a service with a conflicting name that only works on Linux. I suspect most
  # macOS users prefer Keychain.
  options.services.auth-agent = {
    enable = lib.mkEnableOption "SSH agent";
    package = lib.mkPackageOption pkgs "openssh" { };

    socket = lib.mkOption {
      type = lib.types.str;
      default = "ssh-agent.sock";
      description = ''
        The name of the socket to use. The socket will be created in
        <filename>/run/user/$UID/</filename> Linux, or
        <filename>/tmp/</filename> on macOS.
      '';
    };
  };

  config = lib.mkMerge [
    (lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
      systemd.user.services.auth-agent = {
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

    (lib.mkIf (cfg.enable && pkgs.stdenv.isDarwin) {
      launchd.agents.auth-agent = {
        enable = true;
        config = {
          KeepAlive = true;
          RunAtLoad = true;
          ProgramArguments = [
            (
              let
                socket = "/tmp/${cfg.socket}";
                scriptPath = pkgs.writers.writeBash "auth-agent-service" ''
                  ${pkgs.coreutils}/bin/rm -f ${lib.escapeShellArg socket}
                  ${cfg.package}/bin/ssh-agent -D -a ${lib.escapeShellArg socket}
                '';
              in
              toString scriptPath
            )
          ];
        };
      };
    })
  ];
}
