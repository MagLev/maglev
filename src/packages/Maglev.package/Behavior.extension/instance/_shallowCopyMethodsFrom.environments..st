*maglev-runtime
_shallowCopyMethodsFrom: aClass environments: anArray

	self allInstVarNames size >= aClass allInstVarNames size ifFalse:[
		ArgumentError signal: 'copy methods, arg''s class has more fixed instvars than self'
	].
	self superclass == aClass superclass ifFalse: [
		ArgumentError signal: 'copy methods, arg''s superclass isn''t the same as self''s superclass'.
	].
	1 to: 2 do: [:num | |src tgt| 
		num == 1 
			ifTrue: [src := aClass. tgt := self]
			ifFalse: [src := aClass class. tgt := self class].
		anArray do: [:envId |
			(src selectorsForEnvironment: envId) do: [:sel || m |
				m := (src compiledMethodAt: sel environmentId: envId)
						_copyForClass: tgt aliasFrom: nil to: nil comment: '
METHOD COPIED by __copy_methods'.
				m immediateInvariant.
				tgt addRubySelector: sel method: m env: envId].
			tgt _clearLookupCaches: envId]].
