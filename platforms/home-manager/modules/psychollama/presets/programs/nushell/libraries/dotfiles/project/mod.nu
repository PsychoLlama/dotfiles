# Clone and manage git repositories.

# Show module help.
export def 'main' [] {
  help modules project
}

# Clone a GitHub project or open it if it already exists.
export def --env 'project open' [
  # A repository owner/project shorthand. Defaults to GitHub.
  project_id: string@'nu-complete-projects'
] {
  let project = project parse $project_id
  let clone_destination = project path $project

  if not ($clone_destination | path exists) {
    git clone $project.uri $clone_destination
  }

  cd $clone_destination
}

# Determine where a project should exist on disk.
export def 'project path' [
  project: record
] {
  [
    (project root | path expand)
    ($project.owner | str downcase)
    $project.repo
  ] | path join
}

# Get the root directory of all projects.
export def 'project root' [
  suggested_root?: string
] {
  $suggested_root
    | default $env.REPO_ROOT?
    | default ~/projects
    | path expand
}

# Parse a project address and return metadata.
export def 'project parse' [
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
export def 'project list' [] {
  fd . --type d --max-depth 2 --min-depth 2 (project root)
    | lines
    | each {
      path split
        | last 2
        | { owner: $in.0, repo: $in.1 }
    }
}

def 'nu-complete-projects' [] {
  project list | each { $"($in.owner)/($in.repo)" }
}
