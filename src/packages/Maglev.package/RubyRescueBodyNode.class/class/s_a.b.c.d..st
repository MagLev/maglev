*maglev-ast
s_a: ex_list b: body c: next_rescue d: srcOfs
	| res |
	res := self _basicNew.
	ex_list == nil
		ifTrue: [res initStdErrorRescueWith: body and: next_rescue at: srcOfs]
		ifFalse: [
			ex_list class ~= RubySplatNode
				ifTrue: [res initRescueFor: ex_list with: body and: next_rescue at: srcOfs]
				ifFalse: [res initSplatRescue: ex_list with: body and: next_rescue at: srcOfs]].
	^ res
