*maglev-runtime
irNode
  body ifNil:[  ^ GsComLiteralNode newString:'' ]
       ifNotNil:[ :bdy |  ^ bdy irEvaluateBlock ]

