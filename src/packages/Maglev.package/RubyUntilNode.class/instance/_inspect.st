*maglev-runtime
_inspect
  ^ '[:until, ', conditionNode _inspect, ', ' 
         , bodyNode _inspect, ', '
         , self baseSelector _inspect , $]

