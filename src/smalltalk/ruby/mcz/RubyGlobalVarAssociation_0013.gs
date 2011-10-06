
set class RubyGlobalVarAssociation
category: 'as yet unclassified'
method:
aliasTo: otherAssoc
 
  value := otherAssoc globalValueHolder .
  readOnly := otherAssoc readOnly 

%


set class RubyGlobalVarAssociation
category: '(as yet unclassified)'
method:
globalValueHolder 
  | arr |
  (arr := value) ifNil:[ arr := { nil } . value := arr  ].
  ^ arr

%


set class RubyGlobalVarAssociation
category: '(as yet unclassified)'
method:
globalVarValue
  "used to retrieve value of a Ruby global variable , such as with key  $a  .
   see also globalVarValue: "

  | arr |
  (arr := value) ifNil:[ ^ nil ] .
  ^ arr at: 1

%


set class RubyGlobalVarAssociation
category: '(as yet unclassified)'
method:
globalVarValue: aValue 
  "Ruby global variables, with names like $a  , 
   use one-level inderection to their value to allow aliasing of global
   variables.  You must use  globalVarValue: to set the value of such
   a global, and  globalVarValue to fetch it.  See also globalValueHolder. "
  | arr |
  (arr := value) ifNil:[ 
     value := { aValue } . 
    isDefined := true .      
  ] ifNotNil:[  
     arr at: 1 put: aValue
  ].
  ^ aValue 

%


set class RubyGlobalVarAssociation
category: '(as yet unclassified)'
method:
invariantGlobalVarValue: aValue
   | arr |
  (arr := value) ifNil:[ 
     arr := { aValue } .  
     value := arr       
  ] ifNotNil:[  
     arr at: 1 put: aValue
  ].
  isDefined := true .
  arr immediateInvariant .
  self immediateInvariant .
  ^ aValue 

%


set class RubyGlobalVarAssociation
category: 'as yet unclassified'
method:
readOnly
   ^ readOnly

%


set class RubyGlobalVarAssociation
category: '(as yet unclassified)'
method:
rubyGlobalVarValue: aValue 
    "invoked via ruby execution only"
  readOnly ifNotNil:[ NameError signal:'global var ', key , ' cannot be assigned to'].
  ^ self globalVarValue: aValue 

%


set class RubyGlobalVarAssociation
category: 'as yet unclassified'
method:
setReadOnly
  readOnly := true

%

