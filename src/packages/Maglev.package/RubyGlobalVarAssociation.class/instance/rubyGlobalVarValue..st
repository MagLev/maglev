(as yet unclassified)
rubyGlobalVarValue: aValue 
    "invoked via ruby execution only"
  readOnly ifNotNil:[ NameError signal:'global var ', key , ' cannot be assigned to'].
  ^ self globalVarValue: aValue 

