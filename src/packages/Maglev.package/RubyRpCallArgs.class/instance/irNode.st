as yet unclassified
irNode
      "ruby_selector_suffix dependent"
  | ary lst sz  last  |
  lst := list .
  sz := lst size .
  sz <= 1 ifTrue:[
    (sz == 1 and:[ (last := lst at: sz) isSplatNode]) ifTrue:[
       ary := last irNode .
    ] ifFalse:[
       ary := GsComArrayBuilderNode new.
       1 to: sz do:[ :n | ary appendElement: ( lst at: n) irNode ].
    ]
  ]  ifFalse: [ "2 or more"
    ary := GsComArrayBuilderNode new. 
    1 to: sz  - 1 do:[:n |
      ary appendElement: ( lst at: n) irNode
    ].
    last := lst at: sz .
    last isSplatNode ifTrue:[ | snd |
      self ir: ary .
      (snd := GsComSendNode new)
         rcvr: ary ;
         rubySelector: #'__add_arguments#1__' ;
         appendArgument: last irArgNode .
      self ir: snd .
      ^ snd   
    ] ifFalse:[
      ary appendElement: last irNode
    ].
  ].
  self ir: ary.
  ^ ary

