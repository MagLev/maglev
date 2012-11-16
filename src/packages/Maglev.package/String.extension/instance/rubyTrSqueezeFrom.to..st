*maglev-runtime
rubyTrSqueezeFrom: from to: to

"Like rubyTrFrom:to:, but removes duplicate characters from modified sections of self.
Returns self, or nil if self not modified."

|map modSet modified writeIndex prevModChar|
(self size = 0 or:[ from size = 0]) ifTrue:[ ^ self ].

map := from _trMapping: to .
modified := false .
modSet := BitSet forTemplateString: from .
prevModChar := nil .
writeIndex := 1 .
1 to: self size do:[:i| | currentByte |
	currentByte := self codePointAt: i .
	(modSet at: currentByte) = 1 
		ifTrue:[ | newChar |
			modified := true .
			newChar := map at: (currentByte + 1) .
			(newChar ~~ prevModChar) 
				 ifTrue:[
					"This is not a repeated character, so modify"
					self at: writeIndex put: newChar .
					writeIndex := writeIndex + 1 .
					prevModChar := newChar]]
		ifFalse:[
			prevModChar := nil .
			writeIndex ~~ i ifTrue:[ self at: writeIndex put: (self at: i) ] .
			writeIndex := writeIndex + 1 ]].
modified ifTrue:[ self size: (writeIndex - 1) . ^ self ] ifFalse:[ ^ nil ].

