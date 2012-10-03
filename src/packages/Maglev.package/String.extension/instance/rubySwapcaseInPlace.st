*maglev-runtime
rubySwapcaseInPlace

"Return a copy of self with all characters swap cased.  Return nil if receiver unchanged.
  Only works on ascii range a-z and A-Z.  Locale ignorant."
|modified|
modified := false .
1 to: self size do: [ :i ||ch ascii|
	ch := self at: i .
	ascii := ch asciiValue .
	(ascii >= 65 and:[ ascii <= 90] ) ifTrue: [
		modified := true .
		self at: i put: ch asLowercase 
	].
	(ascii >= 97 and:[ ascii <= 122] ) ifTrue: [
		modified := true .
		self at: i put: ch asUppercase
	] .
] .
modified ifTrue:[ ^ self ] ifFalse: [^ nil ] .

