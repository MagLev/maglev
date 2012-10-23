*maglev-ast
new_vcall: rcv sel: selTok
  | result |
  ( result := RubyVCallNode s_a: rcv b: (self check_selector: selTok))
      position: selTok src_offset .
  ^  result

