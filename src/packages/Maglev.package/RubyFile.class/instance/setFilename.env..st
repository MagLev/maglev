*maglev-runtime
setFilename: aString env: envId
  givenPath := aString .
  path := self fullPathFor: aString env: envId .
  loadName ifNil: [ loadName := aString ] .

