{ inputs, den, ... }:

{
  imports = [ inputs.den.flakeModule ];

  # Feeds flake-parts. Defaults to every exposed system without hosts.
  den.systems = import inputs.systems;

  # Exposes `lib`, `inputs`, and `den` to aspect pipeline functions.
  den.default.includes = [ den.batteries.flake-scope ];

  # `aspect` omitted: Den writes `{ }` into class keys, which strict rejects.
  den.schema = {
    host = den.lib.strict;
    user = den.lib.strict;
    home = den.lib.strict;
    flake = den.lib.strict;
  };
}
