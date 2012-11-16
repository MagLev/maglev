*maglev-runtime
removeRubySelector: selectorSymbol env: envId 
  | ns |
  (ns := self nameSpace: envId ) ifNotNil:[
    ns moduleFrozen ifTrue:[
      ArgumentTypeError signal:'attempt to modify a frozen module/class'
    ].
  ].
  ^ super removeRubySelector: selectorSymbol env: envId

