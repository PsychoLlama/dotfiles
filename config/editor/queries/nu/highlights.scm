; INCOMPLETE. This is just enough to get Nushell working in my editor. The
; Treesitter grammar is quite a bit behind, so this is best-effort.
;
; This breaks down particularly around collections, argument types, and
; commands whose identifiers are "strings".

; Basic values
(comment) @comment
(val_string) @string
(val_number) @number
(val_bool) @bool
(val_variable) @variable
(val_nothing) @variable
(param_type
  type: (_) @type)

; Collections
(record_entry key: (identifier) @property)

; Functions
(command head: (cmd_identifier) @function)
(decl_def (cmd_identifier) @function)
(command arg_str: (val_string) @identifier)
((val_closure (parameter_pipes (parameter (identifier) @identifier))))

[
  "and"
  "or"
  "not"
  "def"
  "let"
  "if"
  "else"
  "export"
  "return"
  "do"
] @keyword

[
  "+"
  "-"
  "*"
  "/"
  "="
  "=="
  "!="
  "|"
  ">"
  "<"
  ">="
  "<="
] @operator

; To update this list, run:
;
;     $ scope commands
;       | where is_builtin
;       | get name
;       | each { split row " " | first }
;       | uniq
;       | str join "|"
;
(command head: (cmd_identifier) @function.builtin (#match? @function.builtin "^(alias|all|ansi|any|append|ast|bits|break|bytes|cal|cd|char|clear|collect|columns|commandline|compact|complete|config|const|continue|cp|date|debug|decode|def|default|describe|detect|dfr|do|drop|du|each|echo|encode|enumerate|error|every|exec|exit|explain|explore|export|export-env|extern|fill|filter|find|first|flatten|fmt|for|format|from|generate|get|glob|grid|group|group-by|hash|headers|help|hide|hide-env|histogram|history|http|if|ignore|input|insert|inspect|interleave|into|is-admin|is-empty|is-not-empty|is-terminal|items|join|keybindings|kill|last|lazy|length|let|let-env|lines|load-env|loop|ls|match|math|merge|metadata|mkdir|mktemp|module|move|mut|mv|nu-check|nu-highlight|open|overlay|panic|par-each|parse|path|plugin|port|prepend|print|ps|query|random|range|reduce|register|reject|rename|return|reverse|rm|roll|rotate|run-external|save|schema|scope|select|seq|shuffle|skip|sleep|sort|sort-by|source|source-env|split|split-by|start|stor|str|sys|table|take|tee|term|timeit|to|touch|transpose|try|tutor|ulimit|uname|uniq|uniq-by|update|upsert|url|use|values|version|view|watch|where|which|while|whoami|window|with-env|wrap|zip)$"))

(where_command) @function.builtin
