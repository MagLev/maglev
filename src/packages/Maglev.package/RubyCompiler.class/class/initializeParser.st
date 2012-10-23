*maglev-ast
initializeParser 
  RestrictedClasses ifNil:[ |set |
    (set := IdentitySet new)
      add: ExecBlock ;  add: GsNMethod ; add: VariableContext;
      add: SmallInteger . 
    RestrictedClasses := set .
    (set := IdentitySet new) 
       add: #'+' ; add: #'-' ; add: #'*' ; add:#'>=' ; add:#'<=' ; add:#'bitAnd:'; add:#'<'.
    SmallIntSpecialSelectors := set
  ].

