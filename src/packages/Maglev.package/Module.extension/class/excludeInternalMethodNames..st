*maglev-runtime
excludeInternalMethodNames: set
   "set is an IdentitySet of Symbols. 
    Returns an Array with contents of set filtered
    to exclude most methods beginning with __ , and converted to Strings. "
  | arr destIdx setSize |
  arr := Array new: (setSize := set size) .
  destIdx := 0 .
  1 to: setSize do:[:n || sym |
    sym := set _at: n .
    (sym at: 1 equals: '__' ) ifTrue:[ "exclude internal implementation methods"
	   (sym == #__send__ or:[ sym == #__id__]) ifTrue:[
		  arr at: (destIdx := destIdx + 1) put: sym asString .
		].
    ] ifFalse:[  
      arr at: (destIdx := destIdx + 1) put: sym asString .
    ].
  ] .
  arr size: destIdx .
  ^ arr .

