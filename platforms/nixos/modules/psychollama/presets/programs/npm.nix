{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.npm;
in

{
  options.psychollama.presets.programs.npm = {
    enable = lib.mkEnableOption "Opinionated config for npm";
  };

  config = lib.mkIf cfg.enable {
    environment.etc."/npmrc".text = ''
      ; Fix `npm link` on NixOS.
      prefix = ''${HOME}/.local/share/npm
    '';

    # TODO: Support `environment.variables` in nushell.
    home-manager.sharedModules = [
      {
        # It doesn't respect the global config unless you force it.
        home.sessionVariables.NPM_CONFIG_GLOBALCONFIG = "/etc/npmrc";
      }
    ];
  };
}
