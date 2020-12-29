<div align="center">
  <h1>git.vim</h1>
  <p>A git blame parser with superpowers.</p>
</div>

## Purpose
This is what I imagine `git-blame` would look like if it were built into vim. It works per-line or on a range. It quickly answers questions like "who wrote this segment of code?" or "who's responsible for this file?"

In addition, it supports ignored commit hashes and will recursively scan history until it finds the true source (`git#blame#ignored_commits`). That's particularly useful if your codebase started using an autoformatting library. Less useful if you're using git@>=v2.23.0 and it supports `--ignore-rev` natively.

This plugin also has programmatic tools for working with git repos, such as finding the worktree root or determining if a file is tracked.
