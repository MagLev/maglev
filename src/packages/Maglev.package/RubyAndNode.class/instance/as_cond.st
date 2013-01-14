*maglev-ast
as_cond
  | f s |
  ^ RubyAndNode s_a: ((f := firstNode) ifNotNil:[ f kbegin_value as_cond ])
                  b: ((s := secondNode) ifNotNil:[ s kbegin_value as_cond ])

