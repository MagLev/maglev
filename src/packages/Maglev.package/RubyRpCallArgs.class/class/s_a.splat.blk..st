*maglev-ast
s_a: a splat: splat blk: blk
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: a; 
    append_arg: (RubySplatNode s_a: splat) ;
    append_blk_arg: blk.
  ^ res

