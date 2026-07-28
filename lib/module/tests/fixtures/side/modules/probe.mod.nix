# A second plugin's module. It exists to prove two namespaces mount side
# by side without colliding, and that `inputs` lets a discovered module
# name a peer it has no lexical scope over.
{
  cfg,
  inputs,
  lib,
  ...
}:
{
  options = {
    marker = lib.mkOption {
      type = lib.types.str;
      default = "side";
    };

    # An optional input arrives as `null` unless the assembler supplies
    # it, leaving the plugin to decide what that means.
    label = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = if inputs.label == null then "unlabelled" else inputs.label;
    };
  };

  # A class block is the host, and only the host.
  modules.test.probeSetting = cfg.marker;

  # A peer gets its own block. Inputs arrive verbatim, so naming one is
  # the same gesture a consumer would use: interpolate the plugin.
  plugins."${inputs.main}".services.beta.message = "from the side";
}
