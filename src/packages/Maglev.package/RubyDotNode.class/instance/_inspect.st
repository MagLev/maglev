*maglev-runtime
_inspect
  | sym |
  sym := exclusive ifTrue:[ ':dot3' ] ifFalse:[ ':dot2' ].
  ^ '[' , sym , ', ' , beginNode _inspect , ', ', endNode _inspect, $]

