#!/usr/bin/env -S nu --stdin

# Evaluate the meta-module test suite and report the results.
#
# `lib.runTests` returns only the failures; an empty list means every
# test passed.
export def main [] {
  let failures = nix eval '.#lib.module.tests' --json | from json

  if ($failures | is-empty) {
    print "All module tests passed."
    return
  }

  for failure in $failures {
    print $"FAIL ($failure.name)"
    print $"  expected: ($failure.expected | to nuon)"
    print $"  received: ($failure.result | to nuon)"
  }

  print $"($failures | length) module test\(s) failed."
  exit 1
}
