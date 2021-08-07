{ pkgs ? import <nixpkgs> { }, configDir ? null, }:

let
  rofiWrapper = derivation {
    name = "rofi-wrapper";
    system = pkgs.rofi.system;
    builder = "${pkgs.bash}/bin/bash";
    args = [ builderScript ];

    coreutils = pkgs.coreutils;
    rofi = pkgs.rofi;
    configDir = configDir;
  };

  # Rofi searches $XDG_CONFIG_HOME/rofi for config files.
  builderScript = pkgs.writeScript "rofi-builder" ''
    PATH="$coreutils/bin"

    # I'd prefer buildEnv, but the rofi executable always loses the conflict
    # check. At least this works reliably.
    cp --no-preserve=all -R $rofi $out

    if [[ -d $configDir ]]; then
      mkdir -p $out/rofi-support
      ln -s $configDir $out/rofi-support/rofi
    fi

    cat > $out/bin/rofi << EOF
    #!$builder
    XDG_CONFIG_HOME=$out/rofi-support exec "$rofi/bin/rofi" "\$@"
    EOF

    chmod +x $out/bin/rofi
  '';

in rofiWrapper
