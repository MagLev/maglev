*maglev-runtime
rubyExitOnException: aBoolean
 "Returns previous value of the abort_on_exception state.
  If arg not nil, installs arg as new value."
 | prev tmps |
 tmps := SessionTemps current .
 prev := tmps at:#RubyExitOnException otherwise: false .
 aBoolean ifNotNil:[
   tmps at:#RubyExitOnException put: aBoolean
 ].
 ^ prev

