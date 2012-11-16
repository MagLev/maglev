*maglev-runtime
reload: optionsArray  env: envId
  | ctx sessTmps |
  "resets to empty the persistent env 1 method dicts of 
   all classes for which bootstrap code is loaded.
   
   Does not change persistent LOADED_FEATURES "
  ctx := (sessTmps := SessionTemps current) at: #DefaultRubyContext  otherwise: nil.
  ctx ifNil:[
    (ctx := Saved) ifNil:[ Error signal:'primitives not yet loaded'].
    Boolean useRubyClasses: true bootStrap: true .
    sessTmps installRubyContext: ctx .
    ctx deleteMethods: envId ;
       initTopNameSpace: envId reload: true ;
       setMagLevOptions: optionsArray ; 
       reloadPrimitives: envId . 
    self commitTransaction .
    Boolean useRubyClasses: true bootStrap: false .
    ctx initTransient: envId .
  ] ifNotNil:[
    Error signal:'duplicate reload:env:'
  ].

