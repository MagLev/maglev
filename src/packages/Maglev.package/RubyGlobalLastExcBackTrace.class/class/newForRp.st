as yet unclassified
newForRp

  | node receiver |
  node := self _basicNew .
  receiver := (RubyGlobalLastException _basicNew ) name: #'$!' .
  node rcvr: receiver .
  ^ node

