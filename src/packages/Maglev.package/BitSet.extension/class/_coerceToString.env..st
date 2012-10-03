*maglev-runtime
_coerceToString: anObj env: envId
  ^ [ anObj @ruby1:to_str ] onException: Exception do:[:ex |
      ArgumentTypeError signal: 'coercion with to_str failed'
    ].

