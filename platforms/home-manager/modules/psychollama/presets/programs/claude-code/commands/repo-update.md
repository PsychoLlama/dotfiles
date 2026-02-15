---
description: Update flake inputs and custom packages, then address deprecations.
disable-model-invocation: true
---

!`just update`

## Steps

1. Run checks.
2. Fix failures and deprecation warnings.
3. Once everything passes, commit with a changelog.

## Curating a Changelog

- Identify all _used_ packages who's version changed between updates.
- Describe deprecations and meaningful changes to accommodate new versions.
- Use upstream commits to guide your investigation.
