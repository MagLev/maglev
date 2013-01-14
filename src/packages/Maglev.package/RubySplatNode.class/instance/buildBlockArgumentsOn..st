*maglev-runtime
buildBlockArgumentsOn: irNode

  node ifNil:[  "this path might not be used by MRI AST "
    "A ruby block  { |*|  }  ignores all args; generate as  zero arg block"  
  ] ifNotNil:[
    node buildBlockArgumentsOn: irNode .
    irNode setLastArgStar
  ]

