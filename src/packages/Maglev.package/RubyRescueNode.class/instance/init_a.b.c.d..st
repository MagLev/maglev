*maglev-ast
init_a: body b: rescuebody c: elsebody d: srcOfs 
  bodyNode := body .
  rescueBodyNode := rescuebody .
  elseNode := elsebody .
  self position: (srcOfs ifNil:[ rescuebody position ]).

