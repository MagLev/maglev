*maglev-runtime
_rubyEvalBlockArg

  ^ (self _current _rubyThreadDataAt: 6"evalArgsStk") topOrNil 
       ifNotNil:[ :evArgs | evArgs at: 2"block_arg" ].

