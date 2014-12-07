*maglev-runtime
rubyFullName: envId
  "called from Smalltalk code only"
  | ns nam nmc |
  (self isMeta and: [(nmc := self theNonMetaClass) isBehavior])
        ifTrue: [ ^ nmc rubyFullName: envId ].
  nam := name.
  (ns := self nameSpace: envId) ifNotNil:[ nam := ns fullName ].
  (nam isNil or: [nam size == 0]) ifTrue: [ ^ nil ].
  ^ String withAll: nam
