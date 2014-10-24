*maglev-runtime
_fixSuperclasses: envId 

   "runat end of bootstrap"
   "make Symbol have Object as its superclass in Ruby "
   Symbol transientRubySuperclass: envId put: Object ;
          persistentRubySuperclass: envId put: Object .
   "install ProtoObject in the ruby hierarchy"
   Object transientRubySuperclass: envId put: ProtoObject ;
          persistentRubySuperclass: envId put: ProtoObject .
