*maglev-ast
block_append: head tail: tail
  ".y file checks for head/tail nil"
  | hd |
  hd := head kbegin_value "remove_begin" .
  hd class == RubyBlockNode ifFalse:[
    hd := RubyBlockNode s_list: { hd } .
  ].
  hd append_to_block: tail .
  ^ hd   

