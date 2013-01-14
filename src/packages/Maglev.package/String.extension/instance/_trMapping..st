*maglev-runtime
_trMapping: to

"Create a translation map for tr.  Characters in self are mapped to characters in the to string.
If self starts with $^, then the sense is negated. Character ranges (e.g., 'a-z') are expanded.
A nil in the map means to delete that character. If self is the single character $^, then treat
as non-negating."

| map expandedTo expandedFrom defaultCh negating toSiz |

negating := (self size > 1) and: [ (self at: 1) == $^] . "Handle self is '^'"
expandedTo := to sequenceExpand .
expandedFrom := self sequenceExpand: negating. "Note: this will not have the leading $^."
toSiz := expandedTo size .
defaultCh := toSiz == 0 ifTrue:[ nil ] ifFalse: [expandedTo at: toSiz] .

negating
	ifTrue:[
		"We are negating the characters in from.  All characters not in from get
		set to the default character."
		map := Array new: 256 withAll: defaultCh .
		1 to: expandedFrom size do:[:i| | index | 
			index := (expandedFrom codePointAt: i) + 1 .
			"Set all the characters we are not translating back to themsleves."
			map at: index put: (expandedFrom at: i)]]
	ifFalse:[ |  frSiz n index limit chCls  | 
		map := Array new: 256 .
		chCls := Character .
		1 to: 256 do:[:i| map at: i put: (chCls withValue: (i-1)) ] .
		frSiz := expandedFrom size .
		limit := toSiz min: frSiz .
		n := 1 .
		[ n <= limit ] whileTrue:[ | toCh |
		   index := (expandedFrom codePointAt: n) + 1 .
		   toCh := expandedTo at: n .
		   map at: index put: toCh .
		   n := n + 1 ].
	    [ n <= frSiz ] whileTrue:[
			index := (expandedFrom codePointAt: n) + 1 .
			map at: index put: defaultCh .
			n := n + 1 ] .
		].
^ map.

