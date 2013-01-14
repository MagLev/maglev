*maglev-ast
appendTo: arrayNode evstr2dstr: val
  | v |
  (v := val) class == RubyEvStrNode ifTrue:[
     v := RubyDStrNode s_a: { RubyStrNode s_a: String new  .  v } 
  ].
  arrayNode _append: v .
  ^ arrayNode

