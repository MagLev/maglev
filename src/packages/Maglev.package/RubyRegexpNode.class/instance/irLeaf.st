*maglev-runtime
irLeaf
  | lit |
  (lit := regexpLit) ifNil:[
     lit := value string asRegexpWithOptions: options .
  ].
  ^ GsComLitLeaf new methodLiteral: lit 

