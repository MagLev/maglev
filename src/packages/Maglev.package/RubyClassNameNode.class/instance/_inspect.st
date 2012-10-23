*maglev-runtime
_inspect
  ^ isColon3 == true ifTrue:[ '[:classname, ::', name , $] ]
                    ifFalse:[ '[:classname, ', leftNode _inspect, ', ', name , $] ]

