*maglev-runtime
defTarget: aScope
  "Return a RubyMethodDefTarget, creating one if necessary"
  ^ defTarget ifNil:[ defTarget := RubyMethodDefTarget _basicNew ]

