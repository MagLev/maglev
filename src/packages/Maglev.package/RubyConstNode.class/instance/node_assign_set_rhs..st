*maglev-ast
node_assign_set_rhs: rhs
  "caller responsible for become:"
  | ofs c2n |
  c2n := RubyColon2Node sym: name srcOffset: (ofs := position) .
  ^ (RubyConstDeclNode s_a: c2n b: rhs ) position: ofs .

