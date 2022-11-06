# Upgrade a number of packages to their bleeding edge versions.
#
# Signature: inputs.nixpkgs-unstable => overlay
unstable: self: pkgs: {
  inherit (unstable.legacyPackages.${pkgs.system})
    rofi dunst alacritty zoxide starship tmux fira-code nerdfonts;
}
