*maglev-runtime
persistableInstances: aBoolean
  "true means  newly created classes have non-peristable_instances_bit == 0"
  RubyCompilerState current persistableInstances: aBoolean .
  ^ aBoolean 

