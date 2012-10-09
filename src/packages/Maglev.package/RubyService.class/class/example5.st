examples
example5
	"Demo using implicit blocks"
	| ruby envKeys |
	ruby := RubyService new.
	
	envKeys := (ruby resolve: 'ENV') keys.
	^ envKeys map: [:element | element to_sym]

