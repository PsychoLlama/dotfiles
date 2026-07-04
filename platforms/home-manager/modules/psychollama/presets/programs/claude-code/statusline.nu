# Claude Code statusline: context usage + cost on the left, rate limits on the
# right. Reads the session JSON from stdin (`$in`, bound by the `--stdin` flag
# in the shebang the Nix module injects). Claude Code hands the process its
# stdin as a socket, so `--stdin` — which reads fd 0 directly — is required;
# `open /dev/stdin` would fail because a socket can't be reopened by path.

# Humanize a token count: 30527 -> 30.5k, 200000 -> 200k, 1000000 -> 1M.
def humanize [tokens]: nothing -> string {
  let scaled = if $tokens >= 1_000_000 {
    { value: ($tokens / 1_000_000), unit: "M" }
  } else if $tokens >= 1000 {
    { value: ($tokens / 1000), unit: "k" }
  } else {
    { value: $tokens, unit: "" }
  }
  let rounded = ($scaled.value | math round --precision 1)
  $"($rounded)($scaled.unit)" | str replace --regex '\.0([kM]?)$' '$1'
}

# Terminal width, for right-aligning the RHS group.
def term-width []: nothing -> int {
  $env.COLUMNS? | default 80 | into int
}

# A rate-limit segment: dim label, value colored by how much is consumed.
def rate-limit [label: string, percentage]: nothing -> string {
  let color = if $percentage >= 80 { ansi red } else if $percentage >= 50 { ansi yellow } else { ansi attr_dimmed }
  $"(ansi attr_dimmed)($label): ($color)($percentage)%(ansi reset)"
}

# Left group: context usage + session cost.
def render-left [session]: nothing -> string {
  let context_window = ($session.context_window? | default {})
  let size = ($context_window.context_window_size? | default 0)
  let cost = ($session.cost?.total_cost_usd? | default 0 | into string --decimals 2)

  let context_segment = if $size <= 0 {
    # No API response yet (fresh session or just after /compact).
    $"(ansi attr_dimmed)context —(ansi reset)"
  } else {
    let used = ($context_window.total_input_tokens? | default 0)
    let percentage = ($context_window.used_percentage? | default 0)
    # Dim while there's plenty of room; warn past 15%, alarm past 25%.
    let color = if $percentage < 15 { ansi attr_dimmed } else if $percentage < 25 { ansi yellow } else { ansi red }
    $"($color)(humanize $used)/(humanize $size)(ansi reset)"
  }

  $"($context_segment) | (ansi attr_dimmed)$($cost)(ansi reset)"
}

# Right group: rate-limit usage (Claude.ai subscribers only; absent otherwise).
def render-right [session]: nothing -> string {
  let rate_limits = ($session.rate_limits?)
  if $rate_limits == null { return "" }
  let five_hour = ($rate_limits.five_hour?.used_percentage?)
  let seven_day = ($rate_limits.seven_day?.used_percentage?)
  [
    (if $five_hour != null { rate-limit "5h" $five_hour })
    (if $seven_day != null { rate-limit "7d" $seven_day })
  ] | compact | str join " | "
}

def main [] {
  let session = ($in | from json)
  let left = (render-left $session)
  let right = (render-right $session)

  if ($right | str length) == 0 {
    print -n $left
    return
  }

  # Claude Code reserves a few columns on the right; don't fill to the edge.
  let right_margin = 3
  let width = ((term-width) - $right_margin)
  let left_width = ($left | ansi strip | str length)
  let right_width = ($right | ansi strip | str length)
  let gap = ([1, ($width - $left_width - $right_width)] | math max)
  print -n $"($left)("" | fill --width $gap --character ' ')($right)"
}
