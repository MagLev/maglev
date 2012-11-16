*maglev-runtime
loadCextension: envId
  | lib status fpSiz initName strt end initFct | 
  lib := CLibrary named: fullPath . 
  (status := lib load) _isOneByteString ifTrue:[ RubyLoadError signal: status ].
  cLibrary := lib .
  strt := fullPath indexOfLastByte: $/ asciiValue startingAt: (fpSiz := fullPath size) .
  end := fullPath indexOfLastByte: $. asciiValue startingAt: fpSiz .
  initName := fullPath copyFrom: strt + 1 to: end - 1 .
  initName := 'Init_' , initName .
  initFct := CCallout library: lib name: initName result: #void args: #() .   
  initFct callCextension: nil with: #() block: nil ex: nil .

