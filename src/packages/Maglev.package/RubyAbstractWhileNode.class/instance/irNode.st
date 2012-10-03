accessing
irNode
  |  snd  loopStk res cmState | 
  loopStk := (cmState := RubyCompilerState current)  loopStack .
  loopStk push: self .
  [ | lexLev |
	 lexLev := cmState lexicalLevel .
	 labelBreak := (GsComLabelNode new lexLevel: lexLev argForValue: true)  .  
	 labelNext := ( GsComLabelNode new  lexLevel: lexLev)  . 
	 labelRedo := ( GsComLabelNode new  lexLevel: lexLev)  . 
	 snd := super irNode  .
	 blksInline ifTrue:[ 
		(res := GsComLoopNode new)  send: snd ;  breakLabel:  labelBreak .
	 ] ifFalse:[ 
		res := snd 
     ].
  ] ensure:[
    loopStk pop: self 
  ].
  ^  res 
	

