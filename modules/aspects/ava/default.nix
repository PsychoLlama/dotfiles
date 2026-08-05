{
  inputs,
  dotfiles,
  ...
}:

{
  den.aspects.ava = {
    includes = [
      dotfiles.profiles.full
      dotfiles.profiles.linux-desktop
    ];

    nixos.imports = [
      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
      inputs.nixpkgs.nixosModules.notDetected
      ./_system.nix
    ];
  };
}
