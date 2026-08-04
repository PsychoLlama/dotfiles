# Inspect TCP listeners, like `lsof`/`netstat` but structured.

# List TCP listening sockets as a structured table.
#
# Backed by `ss` (iproute2). Filter with `where` instead of flags:
#   socket | where port == 443
#   socket | where title == nginx
#   socket | where pid == $my_pid
@example "Local listeners" { socket } --result null
export def main []: nothing -> table {
  ^ss --no-header --processes --numeric --listen --tcp
  | lines
  | each { parse-socket }
}

# Parse one `ss --no-header` row into a record.
#
# A TCP-only query omits the `Netid` column, so the layout is:
# State Recv-Q Send-Q Local Peer [Process].
def parse-socket []: string -> record {
  let f = $in | split row --regex '\s+' | where $it != ''
  let local = parse-endpoint $f.3
  let procs = if ($f | length) > 5 { parse-processes $f.5 } else { [] }
  let owner = $procs | first
  let pid = $owner.pid?

  {
    pid: $pid
    address: $local.address
    port: $local.port
    title: $owner.program?
    executable: (if $pid != null { resolve-executable $pid })
    processes: $procs
  }
}

# Resolve a PID's full executable path; null when it can't be read.
#
# `ss` only reports the kernel `comm` name, truncated to 15 characters.
# `/proc/<pid>/exe` has the full path, readable for our own processes
# (others EPERM, which we swallow).
def resolve-executable [pid: int]: nothing -> any {
  let link = ^readlink $"/proc/($pid)/exe" | complete
  if $link.exit_code == 0 {
    $link.stdout | str trim
  }
}

# Split an `address:port` endpoint, handling bracketed IPv6 and `*` ports.
def parse-endpoint [endpoint: string]: nothing -> record {
  # Bracketed IPv6, e.g. `[::1]:631`, `[fd7a::f01]:48423`, `[::]:*`.
  let v6 = $endpoint | parse --regex '^\[(?<address>.*)\](?::(?<port>.*))?$'
  if not ($v6 | is-empty) {
    let r = $v6 | first
    return { address: $r.address, port: (to-port $r.port) }
  }

  # IPv4 / hostname: the port follows the final colon.
  let split = $endpoint | str index-of --end ':'
  if $split < 0 {
    return { address: $endpoint, port: null }
  }

  {
    address: ($endpoint | str substring ..<$split)
    port: (to-port ($endpoint | str substring ($split + 1)..))
  }
}

# Numeric ports become integers; `*` and named services stay as-is.
def to-port [port: string]: nothing -> any {
  if ($port | is-empty) or $port == '*' {
    null
  } else if ($port =~ '^\d+$') {
    $port | into int
  } else {
    $port
  }
}

# Expand the `users:((...))` process column into `{ program, pid }` records.
def parse-processes [column: string]: nothing -> list {
  $column
  | parse --regex '\("(?<program>[^"]+)",pid=(?<pid>\d+),fd=\d+\)'
  | update pid { into int }
}
