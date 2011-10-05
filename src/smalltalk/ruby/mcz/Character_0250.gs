
set class Character
category: '*maglev-runtime'
method:
rubyIsAlphaNumeric
  "Returns true iff receiver is an ascii digit or an ascii letter.
    Character>>isAlphaNumeric returns true for (Character withValue: 255), which
    breaks ruby semantics"
    |ascii|
    ascii := self asciiValue .
    ascii <= 57 ifTrue:[ 
	  ^ ascii >= 48 
	] ifFalse:[
       ascii <= 90 ifTrue:[  
	     ^ ascii >= 65 
	   ] ifFalse:[
	     ^ ascii >= 97 and:[ ascii <= 122]
	   ].
    ].
    ^ false.

%

