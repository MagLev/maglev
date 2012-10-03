*maglev-runtime
_inspect
  ^ '[:while, ', conditionNode _inspect, ', '
         , bodyNode _inspect, ', '
         , self baseSelector _inspect , $]

