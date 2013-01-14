*maglev-runtime
rubyRemoveIv: aString
  "a ruby primitive"
  | rubyName envId ns |
  envId := 1"__callerEnvId" .
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  rubyName := aString asSymbol .
  ns := self nameSpace: envId .
  ns ifNotNil:[ | assoc |
     assoc := ns resolveConstant: rubyName   .
    assoc ifNotNil:[ | val |
      (val := assoc _valueNoAction ) ifNotNil:[ 
          ns removeKey: rubyName .  
         ^ val
       ].
    ].
  ].
  NameError signal:'instance variable ' , rubyName , ' not defined'

