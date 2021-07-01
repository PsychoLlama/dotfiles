{
  pkgs ? import <unstable> {},
  configFile ? pkgs.writeText "empty-file" "",
}:

let
  alacrittyWrapper = derivation {
    name = "alacritty-wrapper";
    system = pkgs.alacritty.system;
    builder = "${pkgs.bash}/bin/bash";
    args = [builderScript];

    coreutils = pkgs.coreutils;
    alacritty = pkgs.alacritty;
    configFile = configFile;
  };

  # Invoke with `--config-file` hard-coded to a value in the nix store.
  builderScript = pkgs.writeScript "alacritty-builder" ''
    PATH="$coreutils/bin"

    mkdir -p $out/bin $out/etc
    cp $configFile $out/etc/alacritty.yml

    cat > $out/bin/alacritty << EOF
    #!$builder
    exec "$alacritty/bin/alacritty" --config-file "$out/etc/alacritty.yml" "\$@"
    EOF

    chmod +x $out/bin/alacritty
  '';

# `alacrittyWrapper` just provides an executable. We still need the real
# alacritty package to generate everything else, like shell completions and
# documentation.
in pkgs.buildEnv {
  name = "alacritty-preconfigured";
  ignoreCollisions = true;

  paths = [
    alacrittyWrapper
    pkgs.alacritty
  ];
}
