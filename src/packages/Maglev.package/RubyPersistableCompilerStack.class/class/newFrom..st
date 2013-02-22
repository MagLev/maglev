as yet unclassified
newFrom: aStack

	^ (aStack collect: [:e | e copy]) changeClassTo: self
