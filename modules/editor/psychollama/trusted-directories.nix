{ config, ... }:

# The editor evaluates in its own isolated module system. `generic.configs`
# (loaded by the editor's builders) supplies `psychollama.trusted-directories`;
# derive `env.trusted` from it so a project-local vimrc beneath a trusted prefix
# is sourced without prompting. `~` is expanded at runtime by the env framework.

{
  config.env.trusted = config.psychollama.trusted-directories;
}
