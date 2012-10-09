*maglev-runtime
parseFileNamed: fullPath loadName: aName
  | ast trace warn  tmps|
  tmps := SessionTemps current .
  trace := tmps at: #MagRpDEBUG otherwise: 0 .
  warn := tmps at:#MAGLEV_parseWarn otherwise: false .
"  useRubyParser == 2 ifTrue:["

   trace ~~ 0 ifTrue:[ GsFile gciLogServer:'Mel parse ' , aName ].  
   ast := RubyParserM rpParseFile: fullPath  loadName: aName yTrace: trace - 1 
        warnings: warn .
 
"
    ] ifFalse:[
  useRubyParser == 0 ifTrue:[
    trace ~~ 0 ifTrue:[ GsFile gciLogServer:'MRI parse ' , aName ].
    (alt := A_ltParser) ifNotNil:[
       alt printSexpForFileNamed: fullPath loadName: aName
    ].
    ast := (parser := self parser) parseFileNamed: fullPath loadName: aName .
    ast fileName: fullPath source:  ( parser catFileNamed: fullPath) .
    ] ifFalse:[
  useRubyParser == 1 ifTrue:[ 
    trace ~~ 0 ifTrue:[ GsFile gciLogServer:'Rp parse ' , aName ].  
    ast := R_ubySexpParser rpParseFile: fullPath loadName: aName
  ] ifFalse:[
    self error:'Ruby parser not specified'
  ]]].
"
  ^ ast  "result is after walkWithScopes has been done"

