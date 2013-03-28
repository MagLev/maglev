*maglev-runtime
addRubySingletonClass: envId

"Insert a new singleton class in the receiver's class hierarchy.
 Returns receiver, or signals an error.
 The new class is persistable.  "

  |vCls prevCls singletonClass|
  prevCls := self virtualClass.
  singletonClass := Metaclass3 _subclass: #Singleton instVarNames: {} format: 0 constraints: {} classVars: SymbolDictionary new poolDictionaries: {}.

  "Why did we need envId here?"
  vCls := self _addRubySingletonClass: singletonClass forSuperClass: self rubySingletonSuperclass.
  vCls destClass: self.
  vCls _isOneByteString ifTrue: [
    ArgumentTypeError signal: 'add singleton class disallowed, ', vCls.
  ].

  "env 1 method dicts left as nil"
  "name spaces left as nil."
  vCls persistentRubySuperclass: 1 put: prevCls .
  vCls transientRubySuperclass: 1 put: prevCls .
  "new singleton class has empty method dicts, so no need to clear lookup caches yet"
  ^ self
