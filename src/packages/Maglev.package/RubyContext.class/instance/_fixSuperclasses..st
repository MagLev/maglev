*maglev-runtime
_fixSuperclasses: envId 

   "runat end of bootstrap"
   "make Symbol have Object as its superclass in Ruby "
   Symbol transientRubySuperclass: envId put: Object ;
          persistentRubySuperclass: envId put: Object .

   "Ruby BasicObject handled by the bootstrap .rb files"
    
