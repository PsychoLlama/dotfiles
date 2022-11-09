let
  # Define a simple `programs.<name>` module that installs and configures an
  # arbitrary package.
  makeProgramModule = pkgName:
    { config, lib, pkgs, ... }:
    let cfg = config.programs.${pkgName};

    in with lib; {
      options.programs.${pkgName} = {
        enable = mkEnableOption "Whether to install ${pkgName}";
        package = mkPackageOption pkgs pkgName { };
      };

      config.home.packages = mkIf cfg.enable [ cfg.package ];
    };

in {
  imports = [
    ./w3m.nix

    (makeProgramModule "dive")
    (makeProgramModule "dogdns")
    (makeProgramModule "fd")
    (makeProgramModule "miniserve")
    (makeProgramModule "nmap")
    (makeProgramModule "sshfs")
    (makeProgramModule "termshark")
    (makeProgramModule "whois")
    (makeProgramModule "xh")
  ];
}
