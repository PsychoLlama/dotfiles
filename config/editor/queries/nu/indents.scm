(record_or_block) @indent.begin
(function_definition) @indent.begin
(if_statement) @indent.begin
(array) @indent.begin

[
  "}"
  "]"
  ")"
] @indent.end

[
  "}"
  "]"
  ")"
] @indent.branch

(comment) @indent.auto
