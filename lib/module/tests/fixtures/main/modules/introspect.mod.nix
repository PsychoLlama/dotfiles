{ global, lib, ... }:
{
  # `global` must expose mounted handles only — never the root's own
  # options (`hostSetting` is declared by the test harness's root eval).
  options.fenced = lib.mkOption {
    type = lib.types.bool;
    readOnly = true;
    default = !(global ? hostSetting);
  };
}
