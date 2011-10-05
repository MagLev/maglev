
set class SessionTemps
category: '*maglev-runtime'
method:
at: aKey putNoStub: aValue

"If the receiver already contains a SymbolAssociation with the given key, this
 makes aValue the value of that SymbolAssociation.  Otherwise, this creates a
 new SymbolAssociation with the given key and value and adds it to the
 receiver.  aKey must be a Symbol.   Returns aValue."

| assoc |
self _validatePrivilegeOld: (self at: aKey otherwise: nil) new: aValue.
assoc := self associationAt: aKey otherwise: nil .
assoc ifNil:[
	  assoc := SymbolAssociation newWithKey: aKey value: aValue .
	  assoc _setNoStubbing .
     self _at: aKey put: assoc .
     ^aValue
].
assoc value: aValue ; _setNoStubbing .
^aValue

%


set class SessionTemps
category: '*maglev-runtime'
method:
installRubyContext: ctx

  self at: #DefaultRubyContext put: ctx ;
       at:#RubyPinnedClasses put: (IdentitySet new _setNoStubbing) .

%

