{
  imports = [
    (import ../_mk-unstable-preset.nix "nushell")

    ../../../platform/homeManager/programs/nushell/abbreviations.nix
    ../../../platform/homeManager/programs/nushell/libraries.nix
  ];

  flake.modules.homeManager.default =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      zoxideCommandSetup = pkgs.runCommand "zoxide-init" { buildInputs = [ pkgs.unstable.zoxide ]; } ''
        zoxide init nushell > "$out"
      '';

      # Some modules use POSIX interpolation, which Nushell obviously doesn't
      # support. Just ignore them.
      safeSessionVariables = lib.filterAttrs (
        _: value: !(lib.isString value && lib.strings.hasInfix "\${" value)
      ) config.home.sessionVariables;
    in

    {
      programs.nushell = {
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

          load-env ${lib.hm.nushell.toNushell { } safeSessionVariables}
        '';

        extraEnv = ''
          source ${./env.nu}
        '';
      };

      # The default completions are incompatible with newer versions of Nushell.
      programs.zoxide.enableNushellIntegration = false;
    };
}
