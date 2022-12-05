# Nushell Environment Config File
# The prompt indicators are environmental variables that represent
# the state of the prompt
let-env PROMPT_INDICATOR = { "" }
let-env PROMPT_INDICATOR_VI_INSERT = { "" }
let-env PROMPT_INDICATOR_VI_NORMAL = { "" }
let-env PROMPT_MULTILINE_INDICATOR = { "" }

# TODO: Pull these from `home.sessionVariables`.
let-env EDITOR = "nvim"
let-env MANPAGER = "nvim -c 'Man!'"
let-env SSH_AUTH_SOCK = "/tmp/ssh-agent.sock"

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
let-env ENV_CONVERSIONS = {
  "PATH": {
    from_string: { |s| $s | split row (char esep) | path expand -n }
    to_string: { |v| $v | path expand -n | str collect (char esep) }
  }
}
