let-env config = {
  edit_mode: vi
  shell_integration: true
  show_banner: false
  completions: {
    case_sensitive: false
    partial: false
    quick: true
    external: {
      enable: true
    }
  }

  hooks: {
    pre_prompt: [{
      code: "
        let direnv = (direnv export json | from json)
        let direnv = if ($direnv | length) == 1 { $direnv } else { {} }
        $direnv | load-env
      "
    }]
  }
}


module completions {
  # Custom completions for external commands (those outside of Nushell)
  # Each completions has two parts: the form of the external command, including its flags and parameters
  # and a helper command that knows how to complete values for those flags and parameters
  #
  # This is a simplified version of completions for git branches and git remotes
  def "nu-complete git branches" [] {
    ^git branch | lines | each { |line| $line | str replace '[\*\+] ' '' | str trim }
  }

  def "nu-complete git remotes" [] {
    ^git remote | lines | each { |line| $line | str trim }
  }

  # Download objects and refs from another repository
  export extern "git fetch" [
    repository?: string@"nu-complete git remotes" # name of the repository to fetch
    branch?: string@"nu-complete git branches" # name of the branch to fetch
    --all                                         # Fetch all remotes
    --append(-a)                                  # Append ref names and object names to .git/FETCH_HEAD
    --atomic                                      # Use an atomic transaction to update local refs.
    --depth: int                                  # Limit fetching to n commits from the tip
    --deepen: int                                 # Limit fetching to n commits from the current shallow boundary
    --shallow-since: string                       # Deepen or shorten the history by date
    --shallow-exclude: string                     # Deepen or shorten the history by branch/tag
    --unshallow                                   # Fetch all available history
    --update-shallow                              # Update .git/shallow to accept new refs
    --negotiation-tip: string                     # Specify which commit/glob to report while fetching
    --negotiate-only                              # Do not fetch, only print common ancestors
    --dry-run                                     # Show what would be done
    --write-fetch-head                            # Write fetched refs in FETCH_HEAD (default)
    --no-write-fetch-head                         # Do not write FETCH_HEAD
    --force(-f)                                   # Always update the local branch
    --keep(-k)                                    # Keep dowloaded pack
    --multiple                                    # Allow several arguments to be specified
    --auto-maintenance                            # Run 'git maintenance run --auto' at the end (default)
    --no-auto-maintenance                         # Don't run 'git maintenance' at the end
    --auto-gc                                     # Run 'git maintenance run --auto' at the end (default)
    --no-auto-gc                                  # Don't run 'git maintenance' at the end
    --write-commit-graph                          # Write a commit-graph after fetching
    --no-write-commit-graph                       # Don't write a commit-graph after fetching
    --prefetch                                    # Place all refs into the refs/prefetch/ namespace
    --prune(-p)                                   # Remove obsolete remote-tracking references
    --prune-tags(-P)                              # Remove any local tags that do not exist on the remote
    --no-tags(-n)                                 # Disable automatic tag following
    --refmap: string                              # Use this refspec to map the refs to remote-tracking branches
    --tags(-t)                                    # Fetch all tags
    --recurse-submodules: string                  # Fetch new commits of populated submodules (yes/on-demand/no)
    --jobs(-j): int                               # Number of parallel children
    --no-recurse-submodules                       # Disable recursive fetching of submodules
    --set-upstream                                # Add upstream (tracking) reference
    --submodule-prefix: string                    # Prepend to paths printed in informative messages
    --upload-pack: string                         # Non-default path for remote command
    --quiet(-q)                                   # Silence internally used git commands
    --verbose(-v)                                 # Be verbose
    --progress                                    # Report progress on stderr
    --server-option(-o): string                   # Pass options for the server to handle
    --show-forced-updates                         # Check if a branch is force-updated
    --no-show-forced-updates                      # Don't check if a branch is force-updated
    -4                                            # Use IPv4 addresses, ignore IPv6 addresses
    -6                                            # Use IPv6 addresses, ignore IPv4 addresses
    --help                                        # Display this help message
  ]

  # Check out git branches and files
  export extern "git checkout" [
    ...targets: string@"nu-complete git branches"   # name of the branch or files to checkout
    --conflict: string                              # conflict style (merge or diff3)
    --detach(-d)                                    # detach HEAD at named commit
    --force(-f)                                     # force checkout (throw away local modifications)
    --guess                                         # second guess 'git checkout <no-such-branch>' (default)
    --ignore-other-worktrees                        # do not check if another worktree is holding the given ref
    --ignore-skip-worktree-bits                     # do not limit pathspecs to sparse entries only
    --merge(-m)                                     # perform a 3-way merge with the new branch
    --orphan: string                                # new unparented branch
    --ours(-2)                                      # checkout our version for unmerged files
    --overlay                                       # use overlay mode (default)
    --overwrite-ignore                              # update ignored files (default)
    --patch(-p)                                     # select hunks interactively
    --pathspec-from-file: string                    # read pathspec from file
    --progress                                      # force progress reporting
    --quiet(-q)                                     # suppress progress reporting
    --recurse-submodules: string                    # control recursive updating of submodules
    --theirs(-3)                                    # checkout their version for unmerged files
    --track(-t)                                     # set upstream info for new branch
    -b: string                                      # create and checkout a new branch
    -B: string                                      # create/reset and checkout a branch
    -l                                              # create reflog for new branch
    --help                                          # Display this help message
  ]

  # Push changes
  export extern "git push" [
    remote?: string@"nu-complete git remotes",      # the name of the remote
    ...refs: string@"nu-complete git branches"      # the branch / refspec
    --all                                           # push all refs
    --atomic                                        # request atomic transaction on remote side
    --delete(-d)                                    # delete refs
    --dry-run(-n)                                   # dry run
    --exec: string                                  # receive pack program
    --follow-tags                                   # push missing but relevant tags
    --force-with-lease: string                      # require old value of ref to be at this value
    --force(-f)                                     # force updates
    --ipv4(-4)                                      # use IPv4 addresses only
    --ipv6(-6)                                      # use IPv6 addresses only
    --mirror                                        # mirror all refs
    --no-verify                                     # bypass pre-push hook
    --porcelain                                     # machine-readable output
    --progress                                      # force progress reporting
    --prune                                         # prune locally removed refs
    --push-option(-o): string                       # option to transmit
    --quiet(-q)                                     # be more quiet
    --receive-pack: string                          # receive pack program
    --recurse-submodules: string                    # control recursive pushing of submodules
    --repo: string                                  # repository
    --set-upstream(-u)                              # set upstream for git pull/status
    --signed: string                                # GPG sign the push
    --tags                                          # push tags (can't be used with --all or --mirror)
    --thin                                          # use thin pack
    --verbose(-v)                                   # be more verbose
    --help                                          # Display this help message
  ]

  # Show, delete, and create branches
  export extern "git branch" [
    --delete(-d): string@"nu-complete git branches" # delete a branch
    -D: string@"nu-complete git branches"           # forcefully delete a branch
    --help                                          # display this help message
  ]
}

# Get just the extern definitions without the custom completion commands
use completions *


#########################
# Convenience Functions #
#########################

# Open the editor.
def n [...files: path] {
  if ($files | length) == 0 {
    nvim $env.PWD
  } else {
    nvim -p $files
  }
}

# Fuzzy find and edit a file.
def nf [] {
  let selected_file = (fd --type file | fzf)

  if ($selected_file | str length) > 0 {
    nvim ($selected_file | str trim)
  }
}

# Fuzzy find and open a directory.
def nt [] {
  let selected_dir = (fd --type directory | fzf)

  if ($selected_dir | str length) > 0 {
    nvim ($selected_dir | str trim)
  }
}

# Fuzzy find and open a directory.
def-env cf [] {
  let selected_dir = (fd --type directory | fzf)

  cd (if ($selected_dir | str length) > 0 {
    $selected_dir | str trim
  } else {
    ""
  })
}

# Show information about a nix package.
def gist [pkg_name: string] {
  nix eval --offline --json $"nixpkgs#($pkg_name).meta" | jq -r '.description, .homepage'
}

# `mkdir` and `cd` in one move.
def-env md [directory: path] {
  mkdir $directory
  cd $directory
}

# Show git status.
def s [] {
  let repo_check = (do --ignore-errors {
    git rev-parse --is-inside-work-tree | complete
  })

  if $repo_check.exit_code == 0 {
    git status
  } else {
    echo 'Not a git repo.'
  }
}

# Drop into a shell providing nix packages.
def "," [
  package_name: string # Anything from nixpkgs.
  ...extra_packages: string # Optional extras.
] {
  # Default bare identifiers to the nixpkgs flake.
  def canonicalize [ident: string] {
    if ($ident | str contains '#') {
      $ident
    } else {
      $"nixpkgs#($ident)"
    }
  }

  nix shell (
    | [$package_name] ++ $extra_packages
    | each { |ident| canonicalize $ident }
  )
}

# Drop into a nix development shell.
def ">>" [] {
  nix develop --command $env.SHELL
}

# Encrypt stdin using public keys from GitHub.
def encrypt [
  username: string # Any GitHub username.
] {
  let plaintext = $in
  let keys = (
    | fetch $"https://github.com/($username).keys"
    | lines
    | each { || [ "--recipient" $in ] }
    | flatten
  )

  $plaintext | rage --armor $keys -
}

# Decrypt stdin using the SSH private key.
def decrypt [] {
  rage --decrypt --identity ~/.ssh/id_ed25519 -
}

# Manage git (p)rojects.
def-env p [
  project: string # A GitHub user/project shorthand.
  --root = $"($env.HOME)/projects" # Where to store all the repositories.
  --user = PsychoLlama # Your GitHub username.
  ...extra_git_args # Arguments passed on to `git clone`.
] {
  let resource = (
    [$"($user)/($project)"] ++ $project
    | parse '{user}/{repo}'
    | last
    | upsert user ($in.user | str downcase)
    | upsert repo ($in.repo | str replace '\b\.git$' '')
  )

  let clone_destination = ([$root $resource.user $resource.repo] | path join)

  if not ($clone_destination | path exists) {
    git clone $"git@github.com:($resource.user)/($resource.repo)" $clone_destination $extra_git_args
  }

  cd $clone_destination
}

# Change the desktop wallpaper.
def 'wallpaper set' [image: path] {
  ln -sf $image ~/attic/images/wallpapers/current
  systemctl --user restart swaybg.service
}
