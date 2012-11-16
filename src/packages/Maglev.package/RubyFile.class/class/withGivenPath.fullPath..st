*maglev-runtime
withGivenPath: given fullPath: full 
    "Create a Ruby file with the given path information.  
    Returns nil if the full path does not exist."
    (self isFile: full) ifFalse: [ ^ nil ].
    ^ self new initializeGivenPath: given fullPath: full loadName: given

