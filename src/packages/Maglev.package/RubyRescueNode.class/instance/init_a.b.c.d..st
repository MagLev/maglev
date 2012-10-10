*maglev-ast
init_a: body b: rescuebody c: elsebody d: srcOfs 
  | errorClass conditions currentRescueBodyNode ifClauses revIfClauses ifNode aSrcOfs lastException |
  bodyNode := body .
  aSrcOfs := srcOfs ifNil:[ rescuebody position ].

  "First get all the ifConditions ... or'ed for `rescue A, B' form in a list with on node per rescue clause"
  currentRescueBodyNode := rescuebody.
  lastException := RubyGlobalLastException s_a: #'$!'.
  errorClass := RubyVCallNode s_a: lastException b: #class.
  ifClauses := OrderedCollection new.
  [currentRescueBodyNode notNil] whileTrue: [| ifCond ifClause rubyOrNode revCond excNodes |
	conditions := OrderedCollection new.
	currentRescueBodyNode exceptionNodes == nil "no nodes means to rescue StandardError"
		ifTrue: [excNodes := {RubyColon2Node sym: #StandardError srcOffset: aSrcOfs}]
		ifFalse: ["RubyArrayNode" excNodes := currentRescueBodyNode exceptionNodes list].
  	excNodes do: [:e | "for each excNode, we need to call $!.class == (excNode)"
		conditions add: (RubyCallNode s_a: errorClass b: #== c: (RubyRpCallArgs s_a: e))].
	revCond := conditions reverse.
	ifCond := revCond allButFirst inject: revCond first into: [:rhs :cond | RubyOrNode s_a: cond b: rhs].
	ifClauses add: {ifCond. currentRescueBodyNode bodyNode}.
	currentRescueBodyNode := currentRescueBodyNode nextRescueBody].

  "Add a re-raising else statement to the end of the ifclauses"
  ifClauses add: (RubyVCallNode s_a: lastException b: #'__reraise').

  "Combine all the if clauses into if .. elsif .. else statements"
  revIfClauses := ifClauses reverse.
  ifNode := revIfClauses allButFirst inject: revIfClauses first into: [:else :condAndBody |
					RubyIfNode s_a: condAndBody first b: condAndBody second c: else].

  "Build a new rescueBodyNode that catches Exception, then runs the if-statements, which will re-raise the exception if no rescue clause class matches"
  rescueBodyNode := RubyRescueBodyNode
						s_a: (RubyArrayNode _new: (RubyColon2Node sym: #Exception srcOffset: aSrcOfs))
						b: ifNode
						c: nil
						d: aSrcOfs.
  elseNode := elsebody .
  self position: (srcOfs ifNil:[ rescuebody position ]).

