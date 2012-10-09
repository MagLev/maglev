as yet unclassified
instance
  | obj tmps |
  obj := (tmps := SessionTemps current) at:#RUBY_ThreadCriticalMutex otherwise: nil.
  obj ifNil:[ 
	 obj := self forRubyMutualExclusion .
	 tmps at:#RUBY_ThreadCriticalMutex  put: obj
  ].
  ^ obj

