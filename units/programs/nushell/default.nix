{
  rhizome,
  units,
  self,
  define,
  ...
}:

let
  t = rhizome.types;
  inherit (units.platform) home-manager user;
in

{
  # Abbreviations expanded inline at the prompt, by what is typed. Unlike
  # aliases, the expansion is committed to history, keeping substring search
  # reliable.
  abbreviation = define.collection {
    scope = user.scope;
    type = t.attrs-of t.str;
    allow-unread = true;
  };

  unit = define.unit {
    uses.abbreviations = self.abbreviation;

    provides =
      { abbreviations }:
      [
        (home-manager.lib.program "nushell")

        (home-manager.module (
          {
            config,
            lib,
            pkgs,
            ...
          }:

          let
            zoxideCommandSetup =
              pkgs.runCommand "zoxide-init" { buildInputs = [ config.programs.zoxide.package ]; }
                ''
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
              # Use the default aliases, except for `ls` overrides. Nushell has
              # a great `ls` replacement.
              shellAliases = lib.filterAttrs (key: value: key != "l" && key != "ls") config.home.shellAliases // {
                l = "ls --all";
              };

              extraConfig = lib.mkMerge [
                ''
                  source ${./config.nu}
                  source ${zoxideCommandSetup}

                  load-env ${lib.hm.nushell.toNushell { } safeSessionVariables}
                ''

                # Ordered after the rest of the config, which is free to replace
                # `$env.config`. Without it the position depends on module order.
                (lib.mkOrder 1100 ''
                  $env.config.abbreviations = ($env.config.abbreviations | merge ${
                    lib.hm.nushell.toNushell { } (lib.mergeAttrsList abbreviations // { p = "project"; })
                  })
                '')
              ];

              extraEnv = lib.mkMerge [
                ''
                  ### Add custom libraries to the search path ###
                  $env.NU_LIB_DIRS ++= ${lib.hm.nushell.toNushell { } [ (toString ./libraries) ]}
                ''

                ''
                  source ${./env.nu}
                ''
              ];
            };

            # The default completions are incompatible with newer versions of Nushell.
            programs.zoxide.enableNushellIntegration = false;
          }
        ))
      ];
  };
}
