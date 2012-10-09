*maglev-runtime
rubyAutoloadAssociationAtOrNil: aSymbol env: envId
  | rns assoc cls |
  cls := self .
  [ cls ~~ nil ] whileTrue:[ 
    "probe both normal and virtual classes"
     (rns := cls nameSpace: envId) ifNotNil: [
       (assoc := rns resolveConstant: aSymbol) ifNotNil:[
	      assoc isDefined ifNotNil:[
           ^ assoc
         ].
       ] .
     ] .
     cls := cls rubySuperclass: envId .
  ].
  ^ nil

