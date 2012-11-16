*maglev-ast
s_a: a all: ary b: b splat: splat blk: blk
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: a; _appendAll: b ;
    append_arg: (RubySplatNode s_a: splat) ;
    append_blk_arg: blk.
  ^ res

