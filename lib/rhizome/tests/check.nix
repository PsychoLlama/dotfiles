{ lib, pkgs }:

# The rhizome test suite as a buildable check, wired up in `flake.nix`
# as `checks.<system>.rhizome`. The suite itself is pure evaluation —
# `lib.runTests` returns only the failures — so this exists to give it
# an entrypoint that `nix build` and `nix flake check` understand.
#
# To debug a failure by hand, evaluate the suite directly:
#
#   nix eval --impure --json --expr \
#     'import ./lib/rhizome/tests { lib = (import <nixpkgs> { }).lib; }'

let
  failures = import ./default.nix { inherit lib; };

  report = lib.concatMapStringsSep "\n" (failure: ''
    FAIL ${failure.name}
      expected: ${lib.generators.toPretty { multiline = false; } failure.expected}
      received: ${lib.generators.toPretty { multiline = false; } failure.result}
  '') failures;
in

pkgs.runCommandLocal "rhizome-tests" { } (
  if failures == [ ] then
    ''
      echo "All rhizome tests passed."
      touch $out
    ''
  else
    ''
      cat >&2 <<'REPORT'
      ${report}
      REPORT

      echo "${toString (lib.length failures)} rhizome test(s) failed." >&2
      exit 1
    ''
)
