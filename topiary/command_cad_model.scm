; These should not be formatted.
[
    (comment)
    (string)
] @leaf

; Surround spaces
[
  "else"
  "for"
  "if"
  "let"
  "in"
  "*"
  "/"
  "//"
  "+"
  "-"
  "<<"
  ">>"
  "&"
  "|"
  "^"
  ">"
  ">="
  "=="
  "<="
  "<"
  "!="
  "&&"
  "||"
  "->"
  "="
  "&="
  "|="
  "^="
  "&&="
  "||="
  "^^="
  "+="
  "-="
  "**="
  "*="
  "//="
  "/="
  "<<="
  ">>="
  (path)
] @prepend_space @append_space

; Allow blank lines before.
[
    (statement)
    (comment)
] @allow_blank_line_before

; Newlines go after statements.
(
    (statement) @append_spaced_softline
    .
    (comment)* @do_nothing
)
(comment) @append_hardline @prepend_space
("," @prepend_antispace @append_space)
(struct_definition ")" @prepend_antispace)

(procedural_block
  .
  "{" @append_spaced_softline @append_indent_start

  
  "}" @prepend_indent_end
  .
)
