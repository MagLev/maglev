*maglev-runtime
isDefined 
  isDefined ifNil:[  "trigger the auto-define"
    self _valueFor: key inClass: theClass env: env .
    ^ true  "may have installed a transient copy of self"
  ].
  ^ isDefined

