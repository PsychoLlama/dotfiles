{
  self,
  global,
  lib,
  ...
}:
{
  options = {
    # `global` must expose mounted plugins only — never the host's own
    # options (`hostSetting` is declared by the test harness's host eval).
    fenced = lib.mkOption {
      type = lib.types.bool;
      readOnly = true;
      default = !(global ? hostSetting);
    };

    # `self` reaches every module in this plugin without naming its mount
    # point — including ones that stay disabled.
    peerView = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = self.theme.name;
    };
  };
}
