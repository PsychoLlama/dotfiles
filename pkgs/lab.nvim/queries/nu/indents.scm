; extends
(val_record) @indent.begin
(decl_def (block) @indent.begin)
(ctrl_if (block) @indent.begin)
(val_list) @indent.begin
(parameter_bracks) @indent.begin

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
