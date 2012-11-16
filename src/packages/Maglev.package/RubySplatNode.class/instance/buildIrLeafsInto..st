*maglev-runtime
buildIrLeafsInto: anArray
  node ifNil:[
    "A ruby block  { |*|  }  ignores all args; generate as  zero arg block"  
  ] ifNotNil:[
     node buildIrLeafsInto: anArray
  ]

