{ lib }:

/**
  Apply a module to the arguments it asked for. Rhizome modules take a
  fixed, closed set — unlike the host module system, an unrecognised
  argument is an error rather than a lazy `_module.args` lookup, so a
  typo names itself instead of surfacing later as a missing option.

  `description` names the caller in that error and `subject` names its
  kind, since a rhizome module and a class fragment are handed different
  argument sets and should say so.

  # Type

  ```
  applyModule :: {
    description : String,   # e.g. `dotfiles.programs.git`
    subject : String,       # e.g. `Modules`
    available : AttrSet,    # the closed argument set
  } -> Module -> AttrSet
  ```
*/

{
  description,
  subject,
  available,
}:
module:

if !lib.isFunction module then
  module
else
  let
    unknown = lib.attrNames (lib.removeAttrs (lib.functionArgs module) (lib.attrNames available));
  in
  if unknown == [ ] then
    module (lib.intersectAttrs (lib.functionArgs module) available)
  else
    throw "rhizome: ${description} requested unavailable argument(s): ${lib.concatStringsSep ", " unknown}. ${subject} receive only: ${lib.concatStringsSep ", " (lib.attrNames available)}."
