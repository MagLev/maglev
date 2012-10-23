*maglev-ast
s_a: a b: b blk: blk
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: a; append_arg: b ; append_blk_arg: blk.
  ^ res

