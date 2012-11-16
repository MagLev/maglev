*maglev-ast
s_a: ch
    | node sel rcvr |
    ch == $&  ifTrue:[
      (node := RubyNthRefNode _basicNew) matchNumber: 0
    ] ifFalse:[
      ch == $`  ifTrue:[ sel := #pre_match     ] ifFalse:[
      ch == $'  ifTrue:[ sel := #post_match   ] ifFalse:[
      ch == $+  ifTrue:[ sel := #__plus_match  ] ifFalse:[
         RubyParserM signalError:'unrecognized global $' , ch , ' in :back_ref '
      ]]].
      ( node := self _basicNew ) selector: sel
    ].
    (rcvr := RubyVcGlobalNode _basicNew ) name: #'$~' .
    node rcvr: rcvr .
    ^ node

