*maglev-runtime
_rubyEvalVc

  ^ ((self _current _rubyThreadDataAt: 6"evalArgsStk") topValue at: 1"aBinding") context

