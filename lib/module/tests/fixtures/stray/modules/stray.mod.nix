# Reaches a peer plugin through a class block instead of `plugins`.
# Mounted plugins share the host's fixpoint, so this would otherwise
# work by accident.
{ inputs, ... }:
{
  modules.root."${inputs.main}".theme.name = "hijacked";
}
