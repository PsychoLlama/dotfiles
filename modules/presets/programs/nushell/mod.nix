{
  lib,
  pkgs,
  ...
}:

let
  zoxideCommandSetup = pkgs.runCommand "zoxide-init" { buildInputs = [ pkgs.unstable.zoxide ]; } ''
    zoxide init nushell > "$out"
  '';
in

{
  # The swizzle manifest is only useful once nushell is installed, and it used
  # to share this preset's `enable`.
  config.presets.programs.nushell.swizzle.enable = lib.mkDefault true;

  modules.home-manager =
    { config, ... }:

    let
      # Some modules use POSIX interpolation, which Nushell obviously doesn't
      # support. Just ignore them.
      safeSessionVariables = lib.filterAttrs (
        _: value: !(lib.isString value && lib.strings.hasInfix "\${" value)
      ) config.home.sessionVariables;
    in

    {
      programs.nushell = {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.unstable.nushell;

        libraries = {
          enable = true;
          path = [ ./libraries ];
        };

        abbreviations.p = "project";

        # Use the default aliases, except for `ls` overrides. Nushell has
        # a great `ls` replacement.
        shellAliases = lib.filterAttrs (key: value: key != "l" && key != "ls") config.home.shellAliases // {
          l = "ls --all";
        };

        extraConfig = ''
          source ${./config.nu}
          source ${zoxideCommandSetup}

          load-env ${config.lib.nushell.toNushell { } safeSessionVariables}
        '';

        extraEnv = ''
          source ${./env.nu}
        '';
      };

      # The default completions are incompatible with newer versions of Nushell.
      programs.zoxide.enableNushellIntegration = false;
    };
}
