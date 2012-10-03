*maglev-runtime
fullName
  | par nm |
  ((par := self parent) == nil or:[ par myClass == Object]) ifTrue:[ 
    nm := name .
    ^ String withAll: (nm ifNil:['']) 
  ].
  nm := par fullName . 
  nm addAll: '::' ; addAll: (name ifNil: ['#<aModule>']) .
  ^ nm

