{ rhizome }:

# Machine-specific settings. Split from the dotfiles so a consumer can
# mount the opinions without inheriting my hardware, and so a host says
# which plugin it is configuring rather than sitting inside it.
rhizome.plugin {
  src = ./.;
  inputs.dotfiles = throw "hosts: input `dotfiles` is required.";
}
