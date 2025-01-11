# Clone a GitHub project or open it if it already exists.
export def --env go [
  project_id: string # A repository owner/project shorthand. Defaults to GitHub.
] {
  let project = expr $project_id
  let clone_destination = dir $project

  if not ($clone_destination | path exists) {
    git clone $project.uri $clone_destination
  }

  cd $clone_destination
}

# Determine where a project should exist on disk.
export def dir [
  project: record
] {
  [
    (project-root | path expand)
    ($project.owner | str downcase)
    $project.repo
  ] | path join
}

# Get the root directory of all projects.
export def project-root [
  suggested_root?: string
] {
  $suggested_root
    | default ($env | get -i REPO_ROOT)
    | default ~/projects
    | path expand
}

# Parse a repo address and return metadata.
export def expr [
  project_id: string
] {
  # "https://github.com/github/docs/blob/main/file"
  if ($project_id | str starts-with "https://github.com") {
    let data = $project_id
      | parse --regex '^https:..github.com/(?<owner>[^/]+)/(?<repo>[^/]+).*'
      | first

    return {
      host: 'github.com'
      owner: $data.owner
      repo: $data.repo
      uri: $"git@github.com:($data.owner)/($data.repo).git"
    }
  }

  # "username/their-repo"
  let data = $project_id | parse '{owner}/{repo}'
  if not ($data | is-empty) {
    let data = $data | first

    return {
      host: 'github.com'
      owner: $data.owner
      repo: $data.repo
      uri: $"git@github.com:($data.owner)/($data.repo).git"
    }
  }

  error make {
    msg: "Could not parse project ID."
  }
}

# List all projects.
export def list [] {
  fd . --type d --max-depth 2 --min-depth 2 (project-root)
    | lines
    | each {
      path split
        | last 2
        | { owner: $in.0, repo: $in.1 }
    }
}
