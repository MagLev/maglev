*maglev-ast
s_a: a all: ary b: b blk: blk
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: a; _appendAll: b ;
    append_blk_arg: blk.
  ^ res

