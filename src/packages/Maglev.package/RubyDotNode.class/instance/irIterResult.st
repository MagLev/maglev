*maglev-ast
irIterResult 
  | bg en res |
  fixNumCount == 2 ifTrue:[
     ^ self irNode "a Range literal"
  ].
  bg := beginTmp ifNil:[ GsComLiteralNode newInteger: beginNode _value ]
                  ifNotNil:[ GsComVariableNode new leaf: beginTmp leaf ].
  en := endTmp ifNil:[ GsComLiteralNode newInteger: endNode _value ]
                  ifNotNil:[ GsComVariableNode new leaf: endTmp leaf ].
  (res := GsComSendNode new) 
      rcvr:  bg ; 
      stSelector:  self selector ;
      appendArgument:  en .
  self ir: res .
  ^ res 

