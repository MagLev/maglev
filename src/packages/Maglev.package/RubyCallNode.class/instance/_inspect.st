*maglev-runtime
_inspect
 | res |
 res := '
  [:call, ', receiverNode _inspect, 
          ', :', callName, ', ', argsNode _inspect .
  iterNode ifNotNil:[ res addAll: ', ' ; addAll: iterNode _inspect ].
  res add: $]  .
  ^ res

