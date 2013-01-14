*maglev-runtime
buildArgumentsOn: irNode 
    | nodes na  ncolons idx |
    nodes := irArgNodes .
    na := nodes size .
    ncolons := fixedArgCount .  "excludes the splat, if any"
    idx := 1 .
    [ idx <= ncolons ] whileTrue:[
       irNode appendArgument: (nodes at: idx) .
       idx := idx + 1
    ].
    idx <= na ifTrue:[ "args beyond 3 colons or at,after splat"
      | restArray aSend splatPos |
      restArray := GsComArrayBuilderNode new.
      (splatPos := splatPosition) ~~ 0 ifTrue:[
        [ idx < splatPos ] whileTrue:[
           restArray appendElement: (nodes at: idx) .
           idx := idx + 1
        ].
        (aSend := GsComSendNode new) 
           rcvr: restArray ; stSelector: #_rubyAddAll:  ;
           appendArgument: (nodes at: idx) .
        self ir: aSend .
        restArray := aSend .
        idx := idx + 1 .
        [ idx <= na ] whileTrue:[
          (aSend := GsComSendNode new)
             rcvr: restArray ;  stSelector: #_rubyAddLast: ;
             appendArgument: (nodes at: idx) .
          self ir: aSend .
          restArray := aSend .
          idx := idx + 1 .
        ].
      ] ifFalse:[
        [ idx <= na ] whileTrue:[
           restArray appendElement: (nodes at: idx) .
           idx := idx + 1
        ].
      ].
      irNode appendArgument: restArray .
    ].

