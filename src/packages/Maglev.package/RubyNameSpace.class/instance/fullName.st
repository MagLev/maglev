*maglev-runtime
fullName
  | par nm |
  ((par := self parent) == nil or:[ par myClass == Object]) ifTrue:[ 
    nm := name .
    ^ nm == nil ifTrue: [nil] ifFalse: [String withAll: nm] 
  ].
  nm := par fullName . 
  nm addAll: '::' ; addAll: (name ifNil: ['#<aModule>']) .
  ^ nm

