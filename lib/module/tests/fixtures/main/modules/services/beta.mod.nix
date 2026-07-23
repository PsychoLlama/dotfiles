{ lib, ... }:
{
  options.message = lib.mkOption {
    type = lib.types.str;
    default = "";
  };
}
