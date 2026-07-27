{
  self,
  global,
  lib,
  ...
}:
{
  options = {
    # `global` must expose mounted plugins only — never the root's own
    # options (`hostSetting` is declared by the test harness's root eval).
    fenced = lib.mkOption {
      type = lib.types.bool;
      readOnly = true;
      default = !(global ? hostSetting);
    };

    # `global` is keyed by plugin handle, and `self` is this plugin's own
    # entry: the two spellings agree.
    reflexive = lib.mkOption {
      type = lib.types.bool;
      readOnly = true;
      default = global."${self}".theme.name == self.theme.name;
    };
  };
}
