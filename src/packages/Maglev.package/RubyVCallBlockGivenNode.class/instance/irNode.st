*maglev-runtime
irNode
  rcvrIsSelf ifFalse:[ ^ super irNode ].
  ^ RubyBlockGivenNode irNode: self evalRcvr: evalRcvr

