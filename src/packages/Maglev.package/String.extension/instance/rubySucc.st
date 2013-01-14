*maglev-runtime
rubySucc
"Implements  Ruby   succ!  for String "
| idx res carry sz lastAlphaNumericIdx |

sz := self size .
sz == 0 ifTrue: [^ self ] .

lastAlphaNumericIdx := self rubyFindLastAlphaNumeric .
lastAlphaNumericIdx == nil 
	ifTrue: [ 
		"String has no alpha numerics, so increment everything"
		idx := sz . 
		[ idx >= 1 ] whileTrue:[
			carry := self _rubyIncrementCharAt: idx .
  			carry == nil ifTrue:[ ^ self ] .
  			idx := idx - 1 .
		].
	] ifFalse: [|firstAlphaNumericIdx|
		"String has alphanumerics, so increment starting at rightmost alpha numeric"
		firstAlphaNumericIdx := lastAlphaNumericIdx .
		idx := lastAlphaNumericIdx .
		[ idx >= 1 ] whileTrue:[
		     (self at: idx) rubyIsAlphaNumeric ifTrue: [
			  firstAlphaNumericIdx := idx .
			  carry := self _rubyIncrementCharAt: idx .
  			  carry == nil ifTrue:[ ^ self ] 
			] .
  			idx := idx - 1
		].
		carry ~~ nil ifTrue:[ self insertAll: carry at: firstAlphaNumericIdx ] .
		^ self .
	] .
carry ~~ nil ifTrue: [ self insert: carry at: 1 ] .
^ self

