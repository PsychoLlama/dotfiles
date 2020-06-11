with (import <nixpkgs> {});

derivation {
  name = "coc-nvim";
  builder = "${bash}/bin/bash";
  system = builtins.currentSystem;
  args = [ ./builder.bash ];
  plugin = vimPlugins.coc-nvim;
  buildInputs = [ coreutils ];
}
