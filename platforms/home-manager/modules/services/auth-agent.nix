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
  # a service with a conflicting name. This one is kept for compatibility.
  options.services.auth-agent = {
    enable = lib.mkEnableOption "SSH agent";
    package = lib.mkPackageOption pkgs "openssh" { };

    socket = lib.mkOption {
      type = lib.types.str;
      default = "ssh-agent.sock";
      description = ''
        The name of the socket to use. The socket will be created in
        <filename>/run/user/$UID/</filename>.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
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
  };
}
