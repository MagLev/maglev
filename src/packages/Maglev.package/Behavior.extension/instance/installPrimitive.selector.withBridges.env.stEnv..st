*maglev-runtime
installPrimitive: rubyName selector: selString withBridges: bridgeBool env: envId
  stEnv: stEnvId
  "For a primitive with last arg a block, the rubyName should be of
   the form   'select&' as in Array.rb .   "
      "ruby_selector_suffix dependent"

  | selector rNameSiz rubySel  stMeth lastCh firstCh stNargs desc |
  rubyName containsSeparator ifTrue:[ self error:'illegal whitespace in rubySelector for primitive'].
  selector := selString asSymbol.
  stMeth := self lookupSelector: selector .  "env 0 lookup"
  stMeth ifNil:[
    (stEnvId > 0 or:[ self == UndefinedObject or:[ self == Boolean]]) ifTrue:[ | arr |
       "look in envId also, or handle  rubymethods   in NilTF.gs "
       arr := self lookupSelector: selector rubyEnv: envId .
       arr ifNotNil:[ stMeth := arr at: 1 ].
    ].
    stMeth ifNil:[
      self error:'smalltalk method  ' , selString , ' not found in ' , self name ,
        '  for ruby primitive ' , rubyName  .
      ^ self
    ].
  ].
  rNameSiz := rubyName size .
  lastCh := rubyName at: rNameSiz .
  firstCh := rubyName at: 1 .     "need to exclude binary selectors like **  "
  stNargs := stMeth numArgs .
  (rNameSiz > 1 and:[ (lastCh == $& or:[ lastCh == $* ])
               and:[ firstCh isLetter or:[ firstCh == $_ ] ] ] ) ifTrue:[
    | nameSuffixSize |
    lastCh == $& ifTrue:[  
      (rubyName at: rNameSiz - 1 ) == $* 
             ifTrue:[ desc := 3 . nameSuffixSize := 2 ]   " *& "
	     ifFalse:[ desc := 1 . nameSuffixSize := 1 ] " _& "
    ] ifFalse:[ 
      lastCh == $* ifTrue:[
        desc := 2 . nameSuffixSize := 1  "*_"
      ] ifFalse:[
        desc := 0 . nameSuffixSize := 0  " __ " 
      ]
    ].
    rubySel := rubyName copyFrom: 1 to:  rNameSiz - nameSuffixSize .
    desc := ((stNargs - nameSuffixSize) bitShift: 2 ) + desc .
  ] ifFalse:[
    rubySel := rubyName .       "compute desc for fixed args "
    desc :=  stNargs bitShift: 2 .
  ].
  rubySel := rubySel _asSymbolWithRubySuffix: desc .
  stMeth rubyOptArgsBits ~~ 0 ifTrue:[ self error:'unexpected arg initializers' ].
  RubyCompiler new installBridgeMethodsFor: rubySel in: self
                  argsDescr: nil optArgs: 0 protection: 0
                  primKind: ( bridgeBool ifTrue:[ 1] ifFalse:[ 2 ]  ) env: envId .
  self addRubySelector: rubySel env: envId smalltalkMethod: stMeth  .
  RubyContext default trackRubyPrimitive: stMeth inClass: self rubySel: rubySel env: envId .

