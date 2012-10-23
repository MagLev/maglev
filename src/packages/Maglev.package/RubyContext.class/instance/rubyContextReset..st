*maglev-runtime
rubyContextReset: envId
  "used when forcing reloading of Ruby bootstrap code"
  | set |
  set :=  modifiedStClasses atOrNil: envId  .
  set ifNotNil:[ 
     set do:[:aCls |
       aCls rubyContextReset: envId .
      aCls virtualClass rubyContextReset: envId .
    ].
  ].
  modifiedStClasses at: envId  put: nil .
  rubyPrimMethods at: envId put: nil .

