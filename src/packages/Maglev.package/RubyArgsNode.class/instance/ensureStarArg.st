*maglev-runtime
ensureStarArg
  "for synthesizing *extra arg to a def of eval() ..."
  restArgNode ifNil:[
    restArgNode := (RubyArgumentNode _basicNew identifier: #extra ) .
  ].
  ^ self

