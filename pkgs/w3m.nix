{ pkgs ? import <nixpkgs> { }, keymap ? pkgs.writeText "empty-file" "", }:

let
  w3mWrapper = derivation {
    name = "w3m-wrapper";
    system = pkgs.w3m.system;
    builder = "${pkgs.bash}/bin/bash";
    args = [ builderScript ];

    coreutils = pkgs.coreutils;
    w3m = pkgs.w3m;
    configFile = pkgs.writeText "w3m-config" ''
      keymap_file ${keymap}
    '';
  };

  builderScript = pkgs.writeScript "w3m-builder" ''
    PATH="$coreutils/bin"

    mkdir -p $out/bin $out/w3m-support
    cp $configFile $out/w3m-support/config

    cat > $out/bin/w3m << EOF
    #!$builder
    exec "$w3m/bin/w3m" -config "$out/w3m-support/config" "\$@"
    EOF

    chmod +x $out/bin/w3m
  '';

in pkgs.buildEnv {
  name = "w3m-preconfigured";
  ignoreCollisions = true;

  paths = [ w3mWrapper pkgs.w3m ];
}
