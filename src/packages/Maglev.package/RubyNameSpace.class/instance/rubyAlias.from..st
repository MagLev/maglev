*maglev-runtime
rubyAlias: newKey from: oldKey
  "runtime support for GlobalVarAliasNode, receiver should be
    transient name space for Object "
  | oldAssoc newAssoc  |
  (oldKey at:1) == $$ ifFalse:[ ArgumentError signal:'expected a global variable name'].
  (newKey at:1) == $$ ifFalse:[ ArgumentError signal:'expected a global variable name'].
  (RubyGlobalVarNode rubyAlias: newKey from: oldKey) ifFalse:[
    "not a global with special code generation"
    (self rubyGlobalVarAssoc:  newKey ) aliasTo: (self rubyGlobalVarAssoc: oldKey ) .
  ].
  ^ nil

