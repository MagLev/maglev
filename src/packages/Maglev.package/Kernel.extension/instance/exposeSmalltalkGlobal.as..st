*maglev-runtime
exposeSmalltalkGlobal: aName as: aRubyName
  | assoc cld ns tns sym |
  assoc := System myUserProfile resolveSymbol: (aName asSymbol) .
  assoc ifNil:[ self error:'Smalltalk global ' , aName , ' not found' ].

  cld := GsProcess _current clientData.
  ns := cld
    ifNotNil: [(cld at: 5) last theNonMetaClass "_rubyThreadDataAt:5 -> rtModuleStack"]
    ifNil: [Object].

  ns rubyConstAt: (sym := aRubyName asSymbol) env: 1 put: assoc _value.

  (tns := assoc _value transientNameSpaceForStore: 1) parent ifNil: [
    assoc _value changeNameTo: sym.
    tns _name: sym;parent: (ns transientNameSpaceForStore: 1)].

  ^ assoc _value

