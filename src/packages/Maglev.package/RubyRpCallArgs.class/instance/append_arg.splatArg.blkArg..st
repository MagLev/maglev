*maglev-ast
append_arg: arg splatArg: s blkArg: blk
  "returns receiver"
  ^ self append_arg: arg ;
         append_arg: (RubySplatNode s_a: s) ;
         append_blk_arg: blk .

