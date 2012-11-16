*maglev-runtime
fileNamed: aString env: envId do: aBlock
    "changed to store currentFile in a session temp, not in instVar,
     to avoid write-write conflict on receiver."
    ^ self withFile: (self fileNamed: aString env: envId) do: aBlock .

