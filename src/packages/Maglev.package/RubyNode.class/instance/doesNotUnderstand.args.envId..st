*maglev-runtime
doesNotUnderstand: aSymbol args: anArray envId: envId
  aSymbol == #lineForOffset: ifTrue:[
    ArgumentError signal:'lineForOffset not implemented.'
  ] .
  ^ self error:'a' , self class name , ' doesNotUnderstand  ', aSymbol 

