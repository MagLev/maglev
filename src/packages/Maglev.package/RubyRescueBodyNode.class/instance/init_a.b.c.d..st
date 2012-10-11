*maglev-ast
init_a: ex_list b: body c: next_rescue d: srcOfs 
  | reraise innerRescue innerRescueBody |

  ex_list ~= nil
	ifTrue: [
		innerRescueBody := (RubyRescueBodyNode _basicNew)
							bodyNode: body;
							exceptionNodes: ex_list;
							nextRescueBody: next_rescue;
							position: srcOfs;
							yourself.
		innerRescue := (RubyRescueNode _basicNew)
							bodyNode: (RubyVCallNode s_a: (RubyGlobalLastException s_a: #'$!') b: #'__reraise');
							rescueBodyNode: innerRescueBody;
							position: srcOfs;
							yourself.
		exceptionNodes := RubyArrayNode _new: (RubyColon2Node sym: #Exception srcOffset: srcOfs).
		bodyNode := innerRescue.
		nextRescueBody := nil ]
	ifFalse: [
		exceptionNodes := ex_list .
		bodyNode := body .
		nextRescueBody := next_rescue].
  self position: srcOfs.
