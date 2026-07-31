{ rhizome }:

# The opinions: one module per program, carrying payloads for every
# platform it touches. Knows nothing about hosts.
rhizome.plugin {
  src = ./.;
  classes.editor = "editor";
}
