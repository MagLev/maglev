
set class RubyThreadCriticalMutex class
category: 'as yet unclassified'
method:
instance
  | obj tmps |
  obj := (tmps := SessionTemps current) at:#RUBY_ThreadCriticalMutex otherwise: nil.
  obj ifNil:[ 
	 obj := self forRubyMutualExclusion .
	 tmps at:#RUBY_ThreadCriticalMutex  put: obj
  ].
  ^ obj

%

