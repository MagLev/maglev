*maglev-runtime
compileTimeClass 
  | cls assoc  |
  (assoc := globAssoc) ifNotNil:[
    cls := assoc _valueNoAction .
  ].
  ^ cls  "maybe nil"

