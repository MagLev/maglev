*maglev-runtime
_inspect
 |res|
 res := '
  [:fcall, :', callName, ', ', argsNode _inspect .
  iterNode ifNotNil:[
    res add: ', '; add: iterNode _inspect 
  ].
  res add: $] .
  ^ res

