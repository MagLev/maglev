(as yet unclassified)
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

