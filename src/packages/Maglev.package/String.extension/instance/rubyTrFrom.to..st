*maglev-runtime
rubyTrFrom: from to: to

"Translate characters in self.  from is a set of characters to translate (possibly negated). to
is a set of corresponding characters to translate to.  The first character in from is changed
to the first character in to, and so on.  If there are more characters in from than to, extend to
with its last character.  Modifies self and returns self, unless no changes made, and then returns nil."

| map idx changed selfsiz |

( (selfsiz := self size) == 0 or:[ from size == 0]) ifTrue:[ ^ nil].

changed := false .
map := from _trMapping: to.

"A nil in the map means to remove that character from the string."
idx := 0 .
1 to: selfsiz do:[:i| | current replacement |
	current := ((self codePointAt: i) + 1) . "Characters are zero based"
	replacement := map at: current .
	replacement ifNotNil:[ 
		idx := idx + 1. 
		((self at: idx) == replacement) ifFalse: [ changed := true ] .
		self at: idx put: replacement.
	] .
].

idx ~~ (selfsiz ) ifTrue: [ self size: idx ] .
changed ifTrue: [ ^ self ] ifFalse: [ ^ nil ] .

