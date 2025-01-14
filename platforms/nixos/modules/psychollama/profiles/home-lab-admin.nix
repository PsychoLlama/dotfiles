{ config, lib, ... }:

let
  inherit (config.psychollama.settings) username;
  cfg = config.psychollama.profiles.home-lab-admin;
in

{
  options.psychollama.profiles.home-lab-admin = {
    enable = lib.mkEnableOption ''
      Configure the machine as an admin to the home lab.
      See: https://github.com/PsychoLlama/home-lab/
    '';
  };

  config = lib.mkIf cfg.enable {
    nix.settings = {
      trusted-users = [ username ]; # Needed by `colmena`.
      builders-use-substitutes = true;
    };

    psychollama.presets = {
      programs.ssh.enable = lib.mkDefault true;
    };

    services.openssh.hostKeys = [
      {
        type = "ed25519";
        path = "/root/.ssh/home_lab";
        comment = "Home Lab deploy key";
      }
    ];
  };
}
