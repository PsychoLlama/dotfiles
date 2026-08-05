{ inputs, ... }:

{
  # Reusable feature aspects live under `dotfiles.*`. Entity aspects (hosts,
  # users) stay in `den.aspects`. `true` exports the namespace as
  # `flake.denful.dotfiles` for downstream flakes.
  imports = [ (inputs.den.namespace "dotfiles" true) ];

  # Den's export reads `den.ful.dotfiles` unconditionally; the attr only
  # materializes once something defines into it.
  dotfiles = { };
}
