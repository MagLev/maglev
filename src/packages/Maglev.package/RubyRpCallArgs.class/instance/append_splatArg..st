*maglev-ast
append_splatArg: splat 
  "returns receiver"
  ^ self append_arg: (RubySplatNode s_a: splat) 

