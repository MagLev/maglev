*maglev-ast
s_a: v
  | res |
  (res := self _basicNew) list: { } ; append_arg: v .
  ^ res

