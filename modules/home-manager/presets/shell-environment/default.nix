{ config, lib, ... }:

let cfg = config.presets.shell-environment;

in with lib; {
  imports = [
    ./bat.nix
    ./bottom.nix
    ./exa.nix
    ./fd.nix
    ./fzf.nix
    ./git.nix
    ./jq.nix
    ./miniserve.nix
    ./ncspot.nix
    ./neovim.nix
    ./w3m.nix
  ];

  options.presets.shell-environment.enable =
    mkEnableOption "Use an opinionated shell environment";

  config.presets = mkIf cfg.enable {
    bat.enable = mkDefault true;
    bottom.enable = mkDefault true;
    exa.enable = mkDefault true;
    fd.enable = mkDefault true;
    fzf.enable = mkDefault true;
    git.enable = mkDefault true;
    jq.enable = mkDefault true;
    miniserve.enable = mkDefault true;
    ncspot.enable = mkDefault true;
    neovim.enable = mkDefault true;
    w3m.enable = mkDefault true;
  };
}
