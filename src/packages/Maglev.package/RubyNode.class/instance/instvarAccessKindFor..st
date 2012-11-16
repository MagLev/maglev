*maglev-runtime
instvarAccessKindFor: aSymbol
  "Return 0 if aSymbol is an instVar with a fixed offset or can use a dynamicIv bytecode.
        1 if it is a dynamic instVar in an instance of Object or a kind of Behavior"

 | comp cls trc  |
  "trc := UserGlobals at:#IvTrace ."
  comp := self compiler .
  (comp instVarExists: aSymbol with: self) ifFalse:[
    cls := comp currentClass .
    (cls == Object or:[ cls isMetaOrModule]) ifTrue:[
       " trc addLast: { aSymbol . cls .  1} ."
      ^ 1
    ].
  ].
  "trc addLast: { aSymbol . cls . 0}."
  ^ 0

