*maglev-runtime
nextConfFileLine: aFile
  "Return next line not beginning with #, or nil if eof,
   contents of line after # are excluded.  "
   | line |
   [ true ] whileTrue:[
     line := aFile nextLine .
     line ifNil:[ ^ nil ].
     line := line trimWhiteSpace . 
     line size > 0 ifTrue:[
       (line at: 1) ~~ $# ifTrue:[
	      ^ (line copyUpTo: $# ) trimWhiteSpace
	   ].
     ].
   ].

  

