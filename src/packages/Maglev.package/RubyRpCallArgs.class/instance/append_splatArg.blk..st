*maglev-ast
append_splatArg: splat blk: blk
  "returns receiver"
  ^ self append_arg: (RubySplatNode s_a: splat) ;
       append_blk_arg: blk .

