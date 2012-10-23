*maglev-runtime
defaultLoadPath
 | stdPath maglevHome |
  stdPath := { }  .
  maglevHome := System gemEnvironmentVariable: 'MAGLEV_HOME' .
  ^ maglevHome ifNotNil:[
     { (maglevHome, '/lib/ruby/site_ruby/1.8' ) . 
        (maglevHome, '/lib/ruby/site_ruby') . 
        (maglevHome, '/lib/ruby/1.8/') . 
         '.'  }
   ] ifNil:[ { } ].

