as yet unclassified
sourceString
   | str s f |
   (str := sourceString) ifNotNil:[   ^ str ].
    s := (f := RubyCompilerState current fileStack topOrNil)  ifNotNil:[ f source].
    ^ s ifNotNil:[  s, '
###' ].
		

