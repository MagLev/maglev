*maglev-runtime
optionallyBuildImplicitBlockTemp
      "github.com/MagLev/maglev/issues/69"
      "If we have an explicit block argument, s/o in ruby might assign to it. We save the original block's reference
       to a temp under the #implicitBlockTempName, so yield+Proc.new can still work"
      "NOTE: Walking nodes in the scope is fine here, these AST nodes are newly created"
	| blockArg |
	(blockArg := argsNode blockArgNode) ifNotNil: [| explicitBlockVar assignment |
		scope addImplicitBlockTemp.
		explicitBlockVar := (RubyImplicitBlockVarNode s_a: blockArg name).
		explicitBlockVar walkWithScope: scope.
		assignment := RubyLocalAsgnNode s_a: scope implicitBlockTempName b: 0 c: explicitBlockVar.
		assignment walkWithScope: scope.
		self bodyNode prepend_to_block: assignment.
	].

