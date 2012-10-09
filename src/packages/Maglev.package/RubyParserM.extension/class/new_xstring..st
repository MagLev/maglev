*maglev-ast
new_xstring: str
  | knd |
  str ifNil:[
    ^ RubyXStrNode _basicNew _value: '' .
  ].
  knd := str str_dstr_evstr_kind .
  knd == 0 ifTrue:[
    ^ RubyXStrNode _basicNew _value: str strNodeValue 
  ].
  knd == 1 ifTrue:[
    ^ RubyDXStrNode _basicNew list: str list 
  ].
  ^ RubyDXStrNode _basicNew list: { RubyStrNode _basicNew _value: '' . str } 

