*maglev-ast
init: rcvr sel: name_tok args: args srcOffset: ofs
| nam |
receiverNode := rcvr .
argsNode := args .
name_tok ifNil:[
  name := #'[]=' .
  position := ofs .  
] ifNotNil:[ | nm |
  (nm := String withAll: name_tok symval asString) add: $= .
  name := nm asSymbol .
  position := name_tok src_offset .
].
^ self

