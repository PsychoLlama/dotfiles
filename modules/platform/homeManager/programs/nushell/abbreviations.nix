{
  flake.homeModules.platform =
    { config, lib, ... }:

    let
      inherit (lib) types;
      cfg = config.programs.nushell.abbreviations;
    in

    {
      options.programs.nushell.abbreviations = lib.mkOption {
        type = types.attrsOf types.str;
        description = ''
          Abbreviations expanded inline at the prompt. Unlike aliases, the
          expansion is committed to history, keeping substring search reliable.
        '';
        default = { };
        example = {
          gs = "git status";
          ll = "ls -l";
        };
      };

      # Ordered after the rest of the config, which is free to replace
      # `$env.config`. Without it the position depends on module order.
      config.programs.nushell.extraConfig = lib.mkIf (cfg != { }) (
        lib.mkOrder 1100
          # nu
          ''
            $env.config.abbreviations = ($env.config.abbreviations | merge ${lib.hm.nushell.toNushell { } cfg})
          ''
      );
    };
}
