*maglev-runtime
_fixSymbolSuperclass: envId 

   "at end of bootstrap,
      make Symbol have Object as its superclass in Ruby "
   Symbol transientRubySuperclass: envId put: Object ;
          persistentRubySuperclass: envId put: Object .

