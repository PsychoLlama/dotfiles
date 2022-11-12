# TODO: Split apart NixOS and MacOS configs.
{
  imports = [ ../../default.nix ./presets ./programs ./user.nix ];
}
