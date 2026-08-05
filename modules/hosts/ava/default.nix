{
  inputs,
  den,
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

  den.aspects.overlord = {
    includes = [ den.batteries.primary-user ];
  }
  // import ./_overlord.nix;

  den.hosts.x86_64-linux.ava = {
    # `user` manages the OS account, `homeManager` the home directory.
    users.overlord.classes = [
      "user"
      "homeManager"
    ];
  };
}
