*maglev-ast
new_evstr: val
  | knd |
  val ifNil:[
    ^ RubyEvStrNode _basicNew "body left as nil" 
  ].
  knd := val str_dstr_evstr_kind . "MNU here if 'unknown rescue body' "
  knd ifNil:[
    ^ RubyEvStrNode _basicNew body: val .
  ].
  ^ val "val is one of  :str :dstr: evstr"

