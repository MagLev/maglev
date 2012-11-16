*maglev-ast
new_string: val
  val class == RubyEvStrNode ifTrue:[
    ^ RubyDStrNode s_a: { val body  }
  ].
  ^ val

