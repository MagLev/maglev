*maglev-runtime
deleteSmalltalkWrapperFiles
  | dir names  |
  dir := '$MAGLEV_HOME/lib/ruby/site_ruby/1.9/smalltalk' .
  (GsFile isServerDirectory: dir) == true ifTrue:[
    names := GsFile contentsOfDirectory: dir onClient: false .
    names do:[:fn | 
	  (GsFile isServerDirectory: fn ) == false ifTrue:[  "skip . , ..  entries" 
	    (GsFile removeServerFile: fn ) ifNil:[
	       Error signal: 'failed to remove ...',  (fn last: 30) , GsFile lastErrorString 
	    ].
      ].
    ].
    (GsFile removeServerDirectory: dir ) ifNil:[
      Error signal: 'failed to remove .../smalltalk ' , GsFile lastErrorString
    ].
  ].

