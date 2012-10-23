*maglev-runtime
walkWithScope: aScope isDefinedQ: isQ
  "returns nil always"
  | rns assoc envId |
  aScope inBootstrap  ifTrue:[
    envId := RubyCompilerState current envId .
    rns := Object transientNameSpaceForStore: envId .
    assoc := rns resolveConstant: name . "constantLookup in root scope"
    assoc ifNotNil:[
      assoc _valueNoAction isNameSpace ifTrue:[  assoc := nil . "class not created yet"]
    ].
    assoc ifNil:[
       GsFile gciLogServer:'warning dynamic colon3 constant ', (RubyNameSpace pathToString: self pathArray)
    ].
  ].
  assoc ifNil:[ (assoc := RubyConstantRef _basicNew) c3name: name ].
  globAssoc := assoc .
  ^ nil

