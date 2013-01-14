*maglev-runtime
s_a: arg b: srcOfs
  | node rcvr | 
  arg _isSmallInteger ifFalse:[ self error:'nthRef arg must be a Fixnum'].
  (node := self _basicNew )
      matchNumber: arg .
  (rcvr := RubyVcGlobalNode _basicNew)  name: #'$~' .
  node rcvr: rcvr ; position: srcOfs .
  ^ node

