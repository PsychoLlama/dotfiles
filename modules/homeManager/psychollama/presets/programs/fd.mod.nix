{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.fd;
in

{
  config = lib.mkIf cfg.enable {
    # `.git/` isn't covered by any .gitignore (git special-cases it), so
    # `fd --hidden` would otherwise descend the entire internal object tree.
    # The global ignore file keeps it out of every git-visible listing.
    programs.fd.ignores = [ ".git/" ];
    programs.fzf.defaultCommand = "fd --hidden --type f";
  };
}
