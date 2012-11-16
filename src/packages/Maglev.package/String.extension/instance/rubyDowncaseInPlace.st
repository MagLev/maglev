*maglev-runtime
rubyDowncaseInPlace
	"Ensure all characters in self are lowercase. Return nil if no change, otherwise return self."
	| changed |
	changed := false .
	1 to: self size do:[ :i | | ch |
		ch := self at: i .
		ch isUppercase ifTrue:[ 
			changed := true .
			self at: i put: ch asLowercase ]].
	changed ifTrue:[ ^ self ] ifFalse:[ ^ nil ].

