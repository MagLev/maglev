
set class RubyBackRefNode class
category: '*maglev-ast'
method:
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

%


set class RubyBackRefNode
category: '*maglev-ast'
method:
backRefErrorString
  | sel | 
  (sel := selector) == #pre_match ifTrue:[ ^ '$`' ].
  sel == #post_match ifTrue:[ ^ '$'''].
  sel == #__plus_match ifTrue:[ ^ '$+'].

%


set class RubyBackRefNode
category: 'as yet unclassified'
method:
isSameAs: other
  ^ selector == other selector

%


set class RubyBackRefNode
category: 'as yet unclassified'
method:
isSmalltalkSend
  ^ false

%


set class RubyBackRefNode
category: 'as yet unclassified'
method:
selector
  ^ selector

%


set class RubyBackRefNode
category: 'as yet unclassified'
method:
selector: aSymbol
   selector := aSymbol

%


set class RubyBackRefNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:back_ref , ' , selector _inspect , $]

%

