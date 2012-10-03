*maglev-runtime
postWalkForYield
  | s |
  firstNode postWalkForYield .
  (s := secondNode) ifNotNil:[ s postWalkForYield ].

