*maglev-ast
irNode 
  | res val |
  fixNumCount == 2 ifTrue:[
    val := exclusive ifTrue:[ Range from: beginNode _value limit: endNode _value ]
                  ifFalse:[  Range from: beginNode _value to: endNode _value ].
    val immediateInvariant .
    res := GsComLiteralNode newObject: val .
  ] ifFalse:[
    res := super irNode 
  ].
  ^ res 

