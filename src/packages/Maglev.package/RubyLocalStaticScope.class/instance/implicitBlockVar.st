*maglev-runtime
implicitBlockVar
  
  ^ self implicitBlockNotNil ifNil: [ ^ GsComLiteralNode newNil ].

