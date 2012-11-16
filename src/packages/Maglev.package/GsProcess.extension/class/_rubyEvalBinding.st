*maglev-runtime
_rubyEvalBinding

  ^ (self _current _rubyThreadDataAt: 6"evalArgsStk") topValue at: 1"aBinding"

