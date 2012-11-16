*maglev-ast
s_splat: splat blk: blk 
  | res |
  (res := self _basicNew) list: { } ;
    append_arg: (RubySplatNode s_a: splat) ;
    append_blk_arg: blk.
  ^ res

