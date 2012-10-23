examples
example6
	"Demo using block syntax.
	 Compare to
	 	(1 to: 10) inject: 1 into: [:res :next | res / next]
	"
	| ary |
	ary := ((RubyService new resolve: 'Range') new: 1 _: 10).
	^ ary inject: 1 do: [:res :next | res / next]

