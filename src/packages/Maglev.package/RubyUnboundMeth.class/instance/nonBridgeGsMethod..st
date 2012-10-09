*maglev-runtime
nonBridgeGsMethod: envId
  |  m prefix  |
  (m := nonBridgeMeth ) ifNotNil:[ ^ m ].
  m := self _nonBridgeGsMethod: envId .
  m ifNotNil: [ ^ m ].
  prefix := gsmeth selector rubySelectorPrefix .
  NameError signal:'cannot find a non-bridge method for ''' , prefix , ''' in Class ''',
                       (gsmeth inClass @ruby1:name ) , '''' .
  

