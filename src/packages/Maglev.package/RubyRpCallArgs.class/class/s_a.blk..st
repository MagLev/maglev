*maglev-ast
s_a: a blk: blk
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: a; append_blk_arg: blk.
  ^ res

