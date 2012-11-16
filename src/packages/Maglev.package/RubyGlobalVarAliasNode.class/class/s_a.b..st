*maglev-ast
s_a: symA b: symB
  | res |
  (res := self _basicNew)
     newName: (RubySymbolNode _basicNew name: symA) 
     oldName: (RubySymbolNode _basicNew name: symB) .
  ^ res

