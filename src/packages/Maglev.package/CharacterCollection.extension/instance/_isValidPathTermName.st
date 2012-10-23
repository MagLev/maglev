*maglev-runtime
_isValidPathTermName

"Returns true if the receiver is a valid term in a path expression."
| first sz idx |
self == #* ifTrue:[ ^ false ].

"inline ruby variant of  isValidIdentifier"
((sz := self size) > 64 or:[sz == 0]) ifTrue:[ ^false ].

first := self at: 1.  idx := 1 .
first == $@ ifTrue:[ 
	sz == 1 ifTrue:[ ^ false ].
	idx := 2 .
	first := self at: 2 .	
].
(first == $_ or:[first isLetter]) ifFalse:[ ^false ].
idx to: sz do: [:i | | c |
  c := self at: i.
  (c == $_  or:[ c isAlphaNumeric]) ifFalse:[ ^false ].
].
^true

