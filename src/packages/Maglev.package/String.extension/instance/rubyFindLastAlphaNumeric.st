*maglev-runtime
rubyFindLastAlphaNumeric
 "return the index of the last alpha numeric character in receiver.
  return nil if none."
|idx|
idx := self size.
[idx >=1 ] whileTrue: [
	(self at: idx) rubyIsAlphaNumeric ifTrue: [ ^ idx ] .
	idx := idx - 1 .
].
^ nil .

