*maglev-runtime
rubyAlias: newKey from: oldKey
  | dict arr |
  dict := SessionTemps current at:#RubyGlobalNodesDict .
  (arr := dict at: oldKey otherwise: nil ) ifNotNil:[
    "subsequent refs to newKey will get same code generation as oldKey,
     and access the one association for variable named oldKey"
    dict at: newKey put: arr .
    ^ true
  ].
  ^ false

