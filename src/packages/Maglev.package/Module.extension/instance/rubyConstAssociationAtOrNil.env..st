*maglev-runtime
rubyConstAssociationAtOrNil: aSymbol env: envId
  | rns assoc cls |
  cls := self .
  [ cls ~~ nil and:[ cls ~~ Object "fix Trac672"] ] whileTrue:[ 
    "probe both normal and virtual classes"
     (rns := cls nameSpace: envId) ifNotNil: [
       (assoc := rns resolveConstant: aSymbol) ifNotNil:[
          ^ assoc
       ] .
     ] .
     cls := cls rubySuperclass: envId .
  ].
  ^ nil

