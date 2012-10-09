*maglev-runtime
addRubySingletonClass: envId

"Insert a new singleton class in the receiver's class hierarchy.
 Returns receiver, or signals an error.
 The new class is persistable.  "

| vCls prevCls |
prevCls := self virtualClass .
vCls := self _addRubySingletonClass: true envId: envId .
vCls _isOneByteString ifTrue:[
  ArgumentTypeError signal: 'add singleton class disallowed, ', vCls .
].
"env 1 method dicts left as nil"
"name spaces left as nil."
vCls persistentRubySuperclass: envId put: prevCls .
vCls transientRubySuperclass: envId put: prevCls .
  "new singleton class has empty method dicts, so no need to clear
     lookup caches yet"
^ self

