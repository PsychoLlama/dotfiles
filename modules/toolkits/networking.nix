{ config, lib, ... }:

{
  # TODO: Move to a sensible preset module.
  config.programs.wireshark.enable =
    lib.mkDefault config.dotfiles.kitchen-sink.enable;
}
