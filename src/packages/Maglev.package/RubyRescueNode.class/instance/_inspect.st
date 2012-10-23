*maglev-runtime
_inspect
  ^ '[:rescue, ', bodyNode _inspect, ', <rescue>', rescueBodyNode _inspect, 
	       ', <else>' , elseNode _inspect , $]

