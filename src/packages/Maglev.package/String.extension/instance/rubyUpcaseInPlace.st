*maglev-runtime
rubyUpcaseInPlace
	"Ensure all characters in self are uppercase. Return nil if no change, otherwise return self."
	| changed |
	changed := false .
	1 to: self size do:[ :i | | ch |
		ch := self at: i .
		ch isLowercase ifTrue:[ 
			changed := true .
			self at: i put: ch asUppercase ]].
	changed ifTrue:[ ^ self ] ifFalse:[ ^ nil ].

