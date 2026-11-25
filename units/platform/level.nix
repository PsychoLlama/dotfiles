{ rhizome, ... }:

rec {
  host = rhizome.level "host" { within = rhizome.root; };
  user = rhizome.level "user" { within = host; };
}
